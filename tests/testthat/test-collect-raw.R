testthat::test_that("collect_raw_season skips existing files and logs failures", {
  raw_dir <- file.path(tempdir(), paste0("nba-raw-", Sys.getpid()))
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  games <- tibble::tibble(
    game_id = c(1L, 2L, 3L),
    season = 2023L,
    game_date = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  )

  testthat::local_mocked_bindings(
    espn_nba_season_game_ids = function(...) games,
    .env = asNamespace("NBAData")
  )

  key <- NBAData:::.espn_nba_summary_key(
    season = games$season[[3]],
    game_date = NBAData:::.espn_nba_clean_date_key(games$game_date[[3]]),
    game_id = games$game_id[[3]]
  )
  season_dir <- file.path(raw_dir, "2023")
  dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)
  existing_path <- file.path(season_dir, paste0("summary_", key, ".json"))
  jsonlite::write_json(list(existing = TRUE), existing_path, auto_unbox = TRUE)

  fetch_save_fn <- function(game_id, raw_dir, file_path) {
    if (game_id == 2L) {
      stop("boom")
    }
    jsonlite::write_json(list(id = game_id), file_path, auto_unbox = TRUE)
    list(error = NULL)
  }

  out <- collect_raw_season(
    season = 2023,
    raw_dir = raw_dir,
    overwrite = FALSE,
    progress = FALSE,
    quiet = TRUE,
    fetch_save_fn = fetch_save_fn,
    pause_sec = 0
  )

  testthat::expect_equal(nrow(out), 3)
  testthat::expect_true(any(out$status == "saved"))
  testthat::expect_true(any(out$status == "skipped_exists"))
  testthat::expect_true(any(out$status == "failed"))
  testthat::expect_true(file.exists(out$file_path[out$game_id == 1L]))
  testthat::expect_true(file.exists(out$file_path[out$game_id == 3L]))
  testthat::expect_false(file.exists(out$file_path[out$game_id == 2L]))

  testthat::expect_true(dir.exists(season_dir))
})
