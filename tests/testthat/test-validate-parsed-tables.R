testthat::test_that("validate_parsed_tables passes happy path", {
  games <- tibble::tibble(
    game_id = c(1L, 2L),
    game_date = as.Date(c("2023-01-01", "2023-01-02")),
    status_completed = c(TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L),
    team_id = c(10L, 11L, 12L, 13L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L, 2L),
    athlete_id = c(100L, 200L, 201L)
  )

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  testthat::expect_true(out$ok)
  testthat::expect_equal(nrow(out$issues), 0)
})

testthat::test_that("validate_parsed_tables flags team_box row count issues", {
  games <- tibble::tibble(
    game_id = c(1L, 2L),
    game_date = as.Date(c("2023-01-01", "2023-01-02")),
    status_completed = c(TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L),
    team_id = c(10L, 11L, 12L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L),
    athlete_id = c(100L, 200L)
  )

  testthat::expect_error(
    validate_parsed_tables(list(games = games, team_box = team_box, player_box = player_box)),
    "team_box_row_count"
  )
})

testthat::test_that("validate_parsed_tables flags duplicate player keys", {
  games <- tibble::tibble(
    game_id = 1L,
    game_date = as.Date("2023-01-01"),
    status_completed = TRUE
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L),
    team_id = c(10L, 11L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 1L),
    athlete_id = c(100L, 100L)
  )

  testthat::expect_error(
    validate_parsed_tables(list(games = games, team_box = team_box, player_box = player_box)),
    "player_box_duplicate"
  )
})

testthat::test_that("validate_parsed_tables returns missingness report", {
  games <- tibble::tibble(
    game_id = c(1L, 2L),
    game_date = as.Date(c("2023-01-01", "2023-01-02")),
    status_completed = c(TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L),
    team_id = c(10L, NA_integer_, 12L, 13L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L),
    athlete_id = c(100L, 200L)
  )

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  report <- out$missingness$team_box
  testthat::expect_true(all(c("column", "n_missing", "pct_missing") %in% names(report)))
  testthat::expect_true(any(report$column == "team_id"))
  testthat::expect_true(report$pct_missing[report$column == "team_id"] > 0)
})

testthat::test_that("validate_parsed_tables returns sample buckets", {
  games <- tibble::tibble(
    game_id = c(1L, 2L, 3L),
    game_date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    status_completed = c(TRUE, TRUE, TRUE)
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L, 2L, 2L, 3L, 3L),
    team_id = c(10L, 11L, 12L, 13L, 14L, 15L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 2L, 3L),
    athlete_id = c(100L, 200L, 300L)
  )

  out <- validate_parsed_tables(
    tables = list(games = games, team_box = team_box, player_box = player_box),
    warn_only = TRUE
  )
  testthat::expect_true(all(c("early", "mid", "late") %in% names(out$samples)))
  testthat::expect_true(length(out$samples$early) > 0)
  testthat::expect_true(length(out$samples$mid) > 0)
  testthat::expect_true(length(out$samples$late) > 0)
})

value_check_tables <- function() {
  games <- tibble::tibble(
    game_id = 1L,
    game_date = as.Date("2023-01-01"),
    status_completed = TRUE,
    status_name = "STATUS_FINAL",
    home_team_score = 100L,
    away_team_score = 90L
  )
  team_box <- tibble::tibble(
    game_id = c(1L, 1L),
    team_id = c(10L, 11L),
    team_home_away = c("home", "away"),
    team_score = c(100L, 90L),
    field_goals_made = c(40L, 35L),
    field_goals_attempted = c(85L, 80L),
    three_point_field_goals_made = c(10L, 8L),
    three_point_field_goals_attempted = c(30L, 25L),
    free_throws_made = c(10L, 12L),
    free_throws_attempted = c(12L, 15L)
  )
  player_box <- tibble::tibble(
    game_id = c(1L, 1L, 1L),
    athlete_id = c(100L, 101L, 200L),
    team_id = c(10L, 10L, 11L),
    points = c(60L, 40L, 90L)
  )
  list(games = games, team_box = team_box, player_box = player_box)
}

testthat::test_that("value checks pass on consistent tables", {
  out <- validate_parsed_tables(value_check_tables())
  testthat::expect_true(out$ok)
  testthat::expect_equal(nrow(out$issues), 0)
})

testthat::test_that("value checks warn on box score that does not add up", {
  tables <- value_check_tables()
  tables$team_box$free_throws_made[1] <- 2L
  testthat::expect_warning(
    out <- validate_parsed_tables(tables),
    "team_points_mismatch"
  )
  testthat::expect_true(out$ok)
  testthat::expect_equal(unique(out$issues$severity), "warning")
})

testthat::test_that("value checks warn on player points and score mismatches", {
  tables <- value_check_tables()
  tables$player_box$points[1] <- 50L
  tables$games$home_team_score <- 99L
  out <- suppressWarnings(validate_parsed_tables(tables))
  testthat::expect_setequal(out$issues$issue, c("player_points_vs_team", "team_score_vs_games"))
})

testthat::test_that("value checks warn on made > attempted and bad home/away pairing", {
  tables <- value_check_tables()
  tables$team_box$three_point_field_goals_attempted[2] <- 5L
  tables$team_box$team_home_away[2] <- "home"
  out <- suppressWarnings(validate_parsed_tables(tables))
  # Two "home" rows also make the away score look wrong.
  testthat::expect_setequal(
    out$issues$issue,
    c("team_made_gt_attempted", "team_box_home_away", "team_score_vs_games")
  )
})

testthat::test_that("postponed games marked completed are not expected to have box scores", {
  tables <- value_check_tables()
  tables$games <- dplyr::bind_rows(
    tables$games,
    tibble::tibble(
      game_id = 2L,
      game_date = as.Date("2023-01-02"),
      status_completed = TRUE,
      status_name = "STATUS_POSTPONED",
      home_team_score = 0L,
      away_team_score = 0L
    )
  )
  out <- validate_parsed_tables(tables)
  testthat::expect_true(out$ok)
  testthat::expect_equal(nrow(out$issues), 0)
})
