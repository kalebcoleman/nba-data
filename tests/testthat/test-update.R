fake_schedule <- function(season, season_type, start_date, end_date, pause_sec) {
  data.frame(
    game_id = c(401000001L, 401000002L),
    season = 2024L,
    season_type = 2L,
    game_date = as.Date(c("2024-01-15", "2024-01-16")),
    game_date_time = as.POSIXct(c("2024-01-15 19:00:00", "2024-01-16 19:00:00"), tz = "America/New_York"),
    home_team_id = c(1L, 3L),
    away_team_id = c(2L, 4L),
    home_team_abbreviation = c("AAA", "CCC"),
    away_team_abbreviation = c("BBB", "DDD"),
    status_completed = c(TRUE, FALSE),
    status_name = c("STATUS_FINAL", "STATUS_SCHEDULED"),
    status_state = c("post", "pre"),
    stringsAsFactors = FALSE
  )
}

fake_fetch <- function(game_id, raw_dir, file_path) {
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(testthat::test_path("fixtures", "json", "completed_with_boxscore.json"), file_path, overwrite = TRUE)
  list(error = NULL)
}

testthat::test_that("nba_update stores schedule and new completed games, and is idempotent", {
  dir <- withr::local_tempdir()
  db <- file.path(dir, "db.sqlite")
  raw <- file.path(dir, "raw")
  calls <- 0L
  counting_fetch <- function(...) {
    calls <<- calls + 1L
    fake_fetch(...)
  }

  # The fixture has no box score rows, so validation warns about row counts.
  testthat::expect_warning(
    out <- nba_update(
      2024, db_path = db, raw_dir = raw, start_date = "2024-01-15", end_date = "2024-01-16",
      pause_sec = 0, refetch_days = 0, schedule_fn = fake_schedule,
      fetch_save_fn = counting_fetch, quiet = TRUE
    ),
    "team_box_row_count"
  )
  testthat::expect_equal(out$new_game_ids, 401000001L)
  testthat::expect_equal(calls, 1L)

  sched <- nba_schedule(2024, db_path = db)
  testthat::expect_equal(nrow(sched), 2)
  testthat::expect_equal(nrow(nba_schedule(2024, upcoming_only = TRUE, db_path = db)), 1)

  con <- nba_db_connect(db)
  stored <- DBI::dbGetQuery(con, "SELECT game_id FROM games")$game_id
  nba_db_disconnect(con)
  testthat::expect_equal(stored, 401000001L)

  again <- nba_update(
    2024, db_path = db, raw_dir = raw, start_date = "2024-01-15", end_date = "2024-01-16",
    pause_sec = 0, refetch_days = 0, schedule_fn = fake_schedule,
    fetch_save_fn = counting_fetch, quiet = TRUE
  )
  testthat::expect_length(again$new_game_ids, 0)
  testthat::expect_equal(calls, 1L)
})

testthat::test_that("espn_nba_schedule keeps upcoming games and season_game_ids drops them", {
  day <- function(d) {
    sched <- fake_schedule()
    sched[sched$game_date == as.Date(d, "%Y%m%d"), , drop = FALSE]
  }
  sched <- espn_nba_schedule(
    2024, start_date = "2024-01-15", end_date = "2024-01-16", pause_sec = 0, scoreboard_fn = day
  )
  testthat::expect_equal(nrow(sched), 2)
  testthat::expect_setequal(sched$status_state, c("post", "pre"))
})

testthat::test_that("nba_current_season labels seasons by end year", {
  testthat::expect_equal(nba_current_season(as.Date("2026-10-28")), 2027L)
  testthat::expect_equal(nba_current_season(as.Date("2027-04-10")), 2027L)
  testthat::expect_equal(nba_current_season(as.Date("2027-08-01")), 2028L)
})

testthat::test_that("migration to v5 creates the schedule table", {
  db <- file.path(withr::local_tempdir(), "db.sqlite")
  con <- nba_db_connect(db)
  on.exit(nba_db_disconnect(con), add = TRUE)
  nba_db_init(con)
  testthat::expect_equal(nba_db_get_version(con), 5L)
  testthat::expect_true(DBI::dbExistsTable(con, "schedule"))
})

testthat::test_that("nba_team_box returns hoopR-style columns for stored rows", {
  db <- file.path(withr::local_tempdir(), "db.sqlite")
  con <- nba_db_connect(db)
  on.exit(nba_db_disconnect(con), add = TRUE)
  nba_db_init(con)

  team_box <- NBAData:::.espn_nba_apply_schema(
    tibble::tibble(
      game_id = c(1L, 1L), season = 2024L, season_type = 2L,
      game_date = as.Date("2024-01-15"), team_id = c(10L, 11L),
      team_abbreviation = c("AAA", "BBB"), team_home_away = c("home", "away"),
      team_score = c(100L, 90L), field_goals_made = c(40L, 35L)
    ),
    "team_box"
  )
  tables <- list(
    games = NBAData:::.espn_nba_empty_table("games"),
    team_box = team_box,
    player_box = NBAData:::.espn_nba_empty_table("player_box")
  )
  write_parsed_tables(tables, format = "sqlite", mode = "upsert", con = con)

  tb <- nba_team_box(2024, con = con)
  testthat::expect_equal(nrow(tb), 2)
  testthat::expect_true(inherits(tb$game_date, "Date"))
  testthat::expect_equal(tb$team_score[tb$team_home_away == "home"], 100L)
  testthat::expect_true(all(c("field_goals_made", "turnovers", "opponent_team_id") %in% names(tb)))
})
