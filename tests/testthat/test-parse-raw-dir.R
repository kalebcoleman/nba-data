testthat::test_that("parse_raw_dir filters by season and returns tables", {
  raw_dir <- file.path(tempdir(), paste0("nba-raw-", Sys.getpid()))
  season_dir <- file.path(raw_dir, "2023")
  dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)

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
    file.path(season_dir, "summary_2023_20230409_1.json")
  )
  jsonlite::write_json(
    list(header = list(id = "2", season = list(year = 2024, type = 2))),
    file.path(raw_dir, "summary_2024_20240101_2.json")
  )

  out <- espn_nba_parse_raw_dir(2023, raw_dir = raw_dir, progress = FALSE)
  testthat::expect_true(all(c("games", "team_box", "player_box", "file_index") %in% names(out)))
  expected_files <- list.files(season_dir, pattern = "summary_2023_", full.names = TRUE)
  testthat::expect_true(all(out$file_index$file_path %in% expected_files))
  testthat::expect_equal(nrow(out$games), 1)
})
