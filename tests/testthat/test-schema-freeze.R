testthat::test_that("parsed tables match frozen schema and keys", {
  raw_dir <- file.path(tempdir(), paste0("nba-schema-", Sys.getpid()))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  jsonlite::write_json(
    list(header = list(
      id = "1",
      season = list(year = 2023, type = 2),
      competitions = list(
        list(
          date = "2023-04-09T00:00Z",
          status = list(type = list(state = "post", name = "Final", completed = TRUE)),
          competitors = list(
            list(homeAway = "home", score = "100", winner = TRUE, team = list(id = "1")),
            list(homeAway = "away", score = "90", winner = FALSE, team = list(id = "2"))
          )
        )
      )
    )),
    file.path(raw_dir, "summary_2023_20230409_1.json")
  )

  tables <- espn_nba_parse_raw_dir(2023, raw_dir = raw_dir, progress = FALSE)
  tables$games <- .espn_nba_apply_schema(tables$games, "games")
  tables$team_box <- .espn_nba_apply_schema(tables$team_box, "team_box")
  tables$player_box <- .espn_nba_apply_schema(tables$player_box, "player_box")

  schema <- .espn_nba_schema()
  testthat::expect_equal(names(tables$games), names(schema$games))
  testthat::expect_equal(names(tables$team_box), names(schema$team_box))
  testthat::expect_equal(names(tables$player_box), names(schema$player_box))

  testthat::expect_equal(nrow(tables$games), dplyr::n_distinct(tables$games$game_id))
  testthat::expect_equal(
    nrow(tables$team_box),
    nrow(dplyr::distinct(tables$team_box, game_id, team_id))
  )
  testthat::expect_equal(
    nrow(tables$player_box),
    nrow(dplyr::distinct(tables$player_box, game_id, athlete_id))
  )

  testthat::expect_true(is.integer(tables$games$game_id))
  testthat::expect_true(is.integer(tables$games$season))
  testthat::expect_true(inherits(tables$games$game_date, "Date"))
  testthat::expect_true(is.character(tables$player_box$home_away))
  testthat::expect_true(is.integer(tables$player_box$points))
  testthat::expect_true(is.integer(tables$team_box$team_score))
})

testthat::test_that("schema changes come with a version bump", {
  # If this snapshot changes, bump NBA_SCHEMA_VERSION and add a migration
  # that lists the columns it adds.
  testthat::expect_snapshot_value(
    list(version = NBA_SCHEMA_VERSION, schema = .espn_nba_schema()),
    style = "deparse"
  )
})
