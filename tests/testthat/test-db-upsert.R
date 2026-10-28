testthat::test_that("db init and upsert are idempotent", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  db_path <- file.path(tempdir(), paste0("nba-", Sys.getpid(), ".sqlite"))
  con <- nba_db_connect(db = db_path, drv = "sqlite")
  on.exit(nba_db_disconnect(con), add = TRUE)
  nba_db_init(con)

  games <- tibble::tibble(
    game_id = 1L,
    season = 2024L,
    season_type = 2L,
    game_date = as.Date("2024-01-01")
  )
  team_box <- tibble::tibble(
    game_id = 1L,
    team_id = 10L,
    team_score = 100L
  )
  player_box <- tibble::tibble(
    game_id = 1L,
    athlete_id = 99L,
    points = 20L
  )

  tables <- list(
    games = .espn_nba_apply_schema(games, "games"),
    team_box = .espn_nba_apply_schema(team_box, "team_box"),
    player_box = .espn_nba_apply_schema(player_box, "player_box")
  )

  suppressWarnings(write_parsed_tables(
    tables,
    format = "sqlite",
    mode = "upsert",
    con = con
  ))
  suppressWarnings(write_parsed_tables(
    tables,
    format = "sqlite",
    mode = "upsert",
    con = con
  ))

  counts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM games")
  testthat::expect_equal(counts$n[[1]], 1)

  team_box$team_score <- 105L
  tables$team_box <- .espn_nba_apply_schema(team_box, "team_box")
  suppressWarnings(write_parsed_tables(
    tables,
    format = "sqlite",
    mode = "upsert",
    con = con
  ))
  updated <- DBI::dbGetQuery(con, "SELECT team_score FROM team_box WHERE game_id = 1 AND team_id = 10")
  testthat::expect_equal(updated$team_score[[1]], 105L)

  idx <- DBI::dbGetQuery(con, "PRAGMA index_list('games')")
  testthat::expect_true(any(grepl("idx_games_season", idx$name)))
})

testthat::test_that("schema migration adds missing columns", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  db_path <- file.path(tempdir(), paste0("nba-mig-", Sys.getpid(), ".sqlite"))
  con <- nba_db_connect(db = db_path, drv = "sqlite")
  on.exit(nba_db_disconnect(con), add = TRUE)

  NBAData:::.nba_db_create_meta_tables(con)
  DBI::dbExecute(con, "CREATE TABLE games (game_id INTEGER PRIMARY KEY)")
  DBI::dbExecute(con, "CREATE TABLE team_box (game_id INTEGER, team_id INTEGER, PRIMARY KEY(game_id, team_id))")
  DBI::dbExecute(con, "CREATE TABLE player_box (game_id INTEGER, athlete_id INTEGER, PRIMARY KEY(game_id, athlete_id))")
  nba_db_set_version(con, 1L, note = "test v1")

  nba_db_init(con, target_version = NBA_SCHEMA_VERSION)

  team_cols <- DBI::dbGetQuery(con, "PRAGMA table_info(team_box)")
  testthat::expect_true("team_score" %in% team_cols$name)
})

testthat::test_that("upsert writes non-empty team_box and player_box", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  db_path <- file.path(tempdir(), paste0("nba-upsert-", Sys.getpid(), ".sqlite"))
  con <- nba_db_connect(db = db_path, drv = "sqlite")
  on.exit(nba_db_disconnect(con), add = TRUE)
  nba_db_init(con)

  games <- tibble::tibble(
    game_id = c(1L, 2L),
    season = 2024L,
    season_type = 2L,
    game_date = as.Date(c("2024-01-01", "2024-01-02"))
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L),
    team_id = c(10L, 11L, 12L, 13L),
    team_score = c(100L, 98L, 90L, 95L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L),
    athlete_id = c(100L, 101L, 102L),
    points = c(20L, 15L, 30L)
  )

  tables <- list(
    games = .espn_nba_apply_schema(games, "games"),
    team_box = .espn_nba_apply_schema(team_box, "team_box"),
    player_box = .espn_nba_apply_schema(player_box, "player_box")
  )

  suppressWarnings(write_parsed_tables(
    tables,
    format = "sqlite",
    mode = "upsert",
    con = con
  ))

  counts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM team_box")
  testthat::expect_equal(counts$n[[1]], nrow(team_box))
  counts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM player_box")
  testthat::expect_equal(counts$n[[1]], nrow(player_box))

  suppressWarnings(write_parsed_tables(
    tables,
    format = "sqlite",
    mode = "upsert",
    con = con
  ))
  counts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM team_box")
  testthat::expect_equal(counts$n[[1]], nrow(team_box))
  counts <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM player_box")
  testthat::expect_equal(counts$n[[1]], nrow(player_box))
})

testthat::test_that("a failed migration step rolls back and keeps the old version", {
  db_path <- file.path(tempdir(), paste0("nba-mig-fail-", Sys.getpid(), ".sqlite"))
  unlink(db_path)
  con <- nba_db_connect(db = db_path, drv = "sqlite")
  on.exit(nba_db_disconnect(con), add = TRUE)

  # team_box and player_box are missing, so the v2 step fails partway through.
  NBAData:::.nba_db_create_meta_tables(con)
  DBI::dbExecute(con, "CREATE TABLE games (game_id INTEGER PRIMARY KEY)")
  nba_db_set_version(con, 1L, note = "test v1")

  testthat::expect_error(nba_db_init(con, target_version = NBA_SCHEMA_VERSION))
  testthat::expect_equal(nba_db_get_version(con), 1L)
  games_cols <- DBI::dbGetQuery(con, "PRAGMA table_info(games)")$name
  testthat::expect_equal(games_cols, "game_id")
})
