#' Season that a date falls in
#'
#' NBA seasons are labeled by the year they end. Dates from August on belong
#' to the next season.
#'
#' @param date A date
#' @return Integer season year
#' @export
nba_current_season <- function(date = Sys.Date()) {
  date <- as.Date(date)
  year <- as.integer(format(date, "%Y"))
  month <- as.integer(format(date, "%m"))
  if (month >= 8) year + 1L else year
}

#' Bring the database up to date
#'
#' Refreshes the schedule around today, then collects, parses, validates, and
#' upserts every completed game that is not in the database yet, plus games
#' from the last `refetch_days` days. Safe to run repeatedly.
#'
#' By default the schedule is refreshed from three days before the last
#' completed game already in the schedule table (or the season start, on the
#' first run) through `days_ahead` days from today.
#'
#' @param season Season year; defaults to the current season
#' @param db_path SQLite database path
#' @param raw_dir Directory for raw JSON
#' @param start_date Optional start of the schedule refresh window
#' @param end_date Optional end of the schedule refresh window
#' @param days_ahead Days past today to include in the schedule
#' @param pause_sec Seconds to wait between requests
#' @param refetch_days Also refetch stored games from this many days before
#'   today, to pick up ESPN's post-game stat corrections
#' @param schedule_fn Function returning the schedule (for testing)
#' @param fetch_save_fn Function that fetches and saves one summary (for testing)
#' @param quiet Suppress progress messages
#' @return Invisible list with the schedule, newly stored game ids, and
#'   validation results
#' @export
nba_update <- function(season = nba_current_season(),
                       db_path = "data/sql/nbadata.sqlite",
                       raw_dir = "data/raw",
                       start_date = NULL,
                       end_date = NULL,
                       days_ahead = 7,
                       pause_sec = 0.5,
                       refetch_days = 3,
                       schedule_fn = espn_nba_schedule,
                       fetch_save_fn = NULL,
                       quiet = FALSE) {
  season <- as.integer(season)
  say <- function(...) if (!isTRUE(quiet)) message(sprintf(...))

  dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
  con <- nba_db_connect(db = db_path, drv = "sqlite")
  on.exit(nba_db_disconnect(con), add = TRUE)
  nba_db_init(con)

  window <- .espn_nba_default_season_window(season, "regular")
  if (is.null(start_date)) {
    last_done <- DBI::dbGetQuery(
      con,
      "SELECT MAX(game_date) AS d FROM schedule WHERE season = ? AND status_completed = 1",
      params = list(season)
    )$d[[1]]
    start_date <- if (is.na(last_done)) window$start_date else as.Date(last_done) - 3
  }
  if (is.null(end_date)) {
    end_date <- min(Sys.Date() + days_ahead, window$end_date)
  }
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  say("Refreshing %s schedule from %s to %s", season, start_date, end_date)
  schedule <- schedule_fn(season, "regular", start_date = start_date, end_date = end_date, pause_sec = pause_sec)
  if (nrow(schedule) > 0) {
    schedule$updated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    schedule$status_completed <- as.integer(schedule$status_completed)
    .nba_db_upsert_table(con, "schedule", schedule, key_cols = "game_id")
  }

  stored <- DBI::dbGetQuery(con, "SELECT game_id FROM games WHERE season = ?", params = list(season))$game_id
  completed <- schedule[schedule$status_completed %in% 1L, , drop = FALSE]
  completed <- completed[!completed$status_name %in% .espn_nba_unplayed_status, , drop = FALSE]
  is_new <- !completed$game_id %in% stored
  is_recent <- completed$game_date >= Sys.Date() - refetch_days
  new_games <- completed[is_new | is_recent, , drop = FALSE]
  say(
    "%d completed games in window: %d new, %d recent to refetch",
    nrow(completed), sum(is_new), sum(!is_new & is_recent)
  )

  result <- list(schedule = schedule, new_game_ids = integer(), validation = NULL)
  if (nrow(new_games) == 0) {
    return(invisible(result))
  }

  collect_args <- list(
    season = season,
    season_type = "regular",
    raw_dir = raw_dir,
    progress = FALSE,
    quiet = quiet,
    pause_sec = pause_sec,
    games = new_games,
    overwrite = TRUE
  )
  if (!is.null(fetch_save_fn)) collect_args$fetch_save_fn <- fetch_save_fn
  collected <- do.call(collect_raw_season, collect_args)

  ok_files <- collected$file_path[collected$status != "failed"]
  failed <- collected$game_id[collected$status == "failed"]
  if (length(failed) > 0) {
    warning(sprintf("Failed to collect %d games: %s", length(failed), paste(failed, collapse = ", ")), call. = FALSE)
  }
  if (length(ok_files) == 0) {
    return(invisible(result))
  }

  tables <- espn_nba_parse_raw_dir(season, raw_dir = raw_dir, progress = FALSE, files = ok_files)
  result$validation <- validate_parsed_tables(tables, warn_only = TRUE)
  write_parsed_tables(tables, format = "sqlite", mode = "upsert", con = con)

  result$new_game_ids <- as.integer(unique(tables$games$game_id))
  say("Stored %d new games", length(result$new_game_ids))
  invisible(result)
}

#' Team box scores in hoopR's column layout
#'
#' Returns one row per team per game with the column names used by
#' `hoopR::load_nba_team_box()`, so code written against hoopR can read from
#' this database instead.
#'
#' @param seasons Integer vector of seasons; NULL for all
#' @param db_path SQLite database path
#' @param con Optional DBI connection (overrides `db_path`)
#' @return A tibble
#' @export
nba_team_box <- function(seasons = NULL, db_path = "data/sql/nbadata.sqlite", con = NULL) {
  if (is.null(con)) {
    con <- nba_db_connect(db = db_path, drv = "sqlite")
    on.exit(nba_db_disconnect(con), add = TRUE)
  }

  cols <- c(
    "game_id", "season", "season_type", "game_date", "team_id", "team_abbreviation",
    "team_name", "team_display_name", "team_home_away", "team_score", "team_winner",
    "field_goals_made", "field_goals_attempted",
    "three_point_field_goals_made", "three_point_field_goals_attempted",
    "free_throws_made", "free_throws_attempted",
    "offensive_rebounds", "defensive_rebounds", "total_rebounds",
    "assists", "steals", "blocks", "turnovers", "fouls",
    "opponent_team_id", "opponent_team_abbreviation", "opponent_team_score"
  )
  sql <- sprintf("SELECT %s FROM team_box", paste(cols, collapse = ", "))
  if (!is.null(seasons)) {
    sql <- sprintf("%s WHERE season IN (%s)", sql, paste(as.integer(seasons), collapse = ", "))
  }

  out <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
  out$game_date <- as.Date(out$game_date)
  out$team_winner <- as.logical(out$team_winner)
  out[order(out$game_date, out$game_id, out$team_home_away), , drop = FALSE]
}

#' Scheduled games from the database
#'
#' @param seasons Integer vector of seasons; NULL for all
#' @param upcoming_only Only games not yet completed
#' @param db_path SQLite database path
#' @param con Optional DBI connection (overrides `db_path`)
#' @return A tibble
#' @export
nba_schedule <- function(seasons = NULL, upcoming_only = FALSE,
                         db_path = "data/sql/nbadata.sqlite", con = NULL) {
  if (is.null(con)) {
    con <- nba_db_connect(db = db_path, drv = "sqlite")
    on.exit(nba_db_disconnect(con), add = TRUE)
  }
  where <- character()
  if (!is.null(seasons)) where <- c(where, sprintf("season IN (%s)", paste(as.integer(seasons), collapse = ", ")))
  if (isTRUE(upcoming_only)) where <- c(where, "status_completed = 0")
  sql <- "SELECT * FROM schedule"
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))

  out <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
  out$game_date <- as.Date(out$game_date)
  out$status_completed <- as.logical(out$status_completed)
  out[order(out$game_date_time, out$game_id), , drop = FALSE]
}
