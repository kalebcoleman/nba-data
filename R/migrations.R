#' Get current schema version
#'
#' @param con DBI connection
#' @return Integer schema version
#' @export
nba_db_get_version <- function(con) {
  if (!DBI::dbExistsTable(con, "nbadata_meta")) {
    return(0L)
  }
  out <- DBI::dbGetQuery(con, "SELECT MAX(schema_version) AS schema_version FROM nbadata_meta")
  version <- out$schema_version[[1]]
  if (is.na(version)) {
    return(0L)
  }
  as.integer(version)
}

#' Record schema version
#'
#' @param con DBI connection
#' @param version Schema version
#' @param note Optional migration note
#' @return TRUE invisibly
#' @export
nba_db_set_version <- function(con, version, note = NULL) {
  pkg <- tryCatch(utils::packageVersion("NBAData"), error = function(e) NULL)
  pkg <- if (is.null(pkg)) NA_character_ else as.character(pkg)
  DBI::dbExecute(
    con,
    "INSERT INTO nbadata_meta (schema_version, applied_at, package_version, note)
     VALUES (?, ?, ?, ?)",
    params = list(
      as.integer(version),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      pkg,
      note
    )
  )
  invisible(TRUE)
}

#' Apply schema migrations
#'
#' @param con DBI connection
#' @param from Current schema version
#' @param to Target schema version
#' @return TRUE invisibly
#' @export
nba_db_apply_migrations <- function(con, from, to) {
  from <- as.integer(from)
  to <- as.integer(to)
  # Versions 1-4 were created during initial development and all add columns
  # from the current schema, so any older database ends at the v4 schema.
  # New migrations should list the exact columns they add.
  steps <- list(
    list(version = 1L, fn = .nba_db_create_tables_v1, note = "init"),
    list(version = 2L, fn = .nba_db_migrate_to_v2, note = "add missing columns"),
    list(version = 3L, fn = .nba_db_migrate_to_v3, note = "expand team_box schema"),
    list(version = 4L, fn = .nba_db_migrate_to_v4, note = "add lead change metrics to team_box"),
    list(version = 5L, fn = .nba_db_migrate_to_v5, note = "add schedule table")
  )

  for (step in steps) {
    if (from < step$version && to >= step$version) {
      # Each step either applies fully or not at all.
      DBI::dbWithTransaction(con, {
        step$fn(con)
        .nba_db_create_indices(con)
        nba_db_set_version(con, step$version, note = step$note)
      })
      from <- step$version
    }
  }

  if (from < to) {
    stop(sprintf("No migrations defined for target version %s", to), call. = FALSE)
  }

  invisible(TRUE)
}

.nba_db_migrate_to_v2 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_add_missing_columns(con, "games", schema$games)
  .nba_db_add_missing_columns(con, "team_box", schema$team_box)
  .nba_db_add_missing_columns(con, "player_box", schema$player_box)
  invisible(TRUE)
}

.nba_db_migrate_to_v3 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_add_missing_columns(con, "team_box", schema$team_box)
  invisible(TRUE)
}

.nba_db_migrate_to_v4 <- function(con) {
  schema <- .espn_nba_schema()
  .nba_db_add_missing_columns(con, "team_box", schema$team_box)
  invisible(TRUE)
}

# v5: schedule of all games, including ones not yet played. Columns are listed
# explicitly so this migration does not change if the schema map changes.
.nba_db_migrate_to_v5 <- function(con) {
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS schedule (
      game_id INTEGER NOT NULL PRIMARY KEY,
      season INTEGER,
      season_type INTEGER,
      game_date TEXT,
      game_date_time TEXT,
      home_team_id INTEGER,
      away_team_id INTEGER,
      home_team_abbreviation TEXT,
      away_team_abbreviation TEXT,
      status_completed INTEGER,
      status_name TEXT,
      status_state TEXT,
      updated_at TEXT
    )")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_schedule_date ON schedule(game_date)")
  invisible(TRUE)
}

.nba_db_add_missing_columns <- function(con, table, schema) {
  info <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info(%s)", table))
  existing <- info$name
  desired <- names(schema)
  missing <- setdiff(desired, existing)
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }

  for (col in missing) {
    type <- switch(
      schema[[col]],
      integer = "INTEGER",
      numeric = "REAL",
      character = "TEXT",
      logical = "INTEGER",
      date = "TEXT",
      posixct = "TEXT",
      "TEXT"
    )
    table_sql <- as.character(DBI::dbQuoteIdentifier(con, table))
    col_sql <- as.character(DBI::dbQuoteIdentifier(con, col))
    DBI::dbExecute(con, sprintf("ALTER TABLE %s ADD COLUMN %s %s", table_sql, col_sql, type))
  }

  invisible(TRUE)
}
