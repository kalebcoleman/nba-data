#' Collect raw summary JSON for a season
#'
#' @param season Season year
#' @param season_type One of "regular" or "postseason"
#' @param raw_dir Directory to store raw JSON
#' @param overwrite Overwrite existing raw files
#' @param progress Show a progress bar while collecting
#' @param quiet Suppress progress messages
#' @param game_ids Optional integer vector of game_ids to collect
#' @param fetch_save_fn Function that fetches and saves a summary by game_id
#' @param pause_sec Seconds to wait after each request, to limit load on ESPN
#' @return A tibble of collection results
#' @export
collect_raw_season <- function(season,
                               season_type = c("regular", "postseason"),
                               raw_dir = "data/raw",
                               overwrite = FALSE,
                               progress = TRUE,
                               quiet = FALSE,
                               game_ids = NULL,
                               fetch_save_fn = function(game_id, raw_dir, file_path) {
                                 espn_nba_summary_raw_safe(game_id, save_raw = TRUE, raw_dir = raw_dir)
                               },
                               pause_sec = 0.5) {
  season_type <- match.arg(season_type)
  season <- as.integer(season)
  season_dir <- if (basename(raw_dir) == as.character(season)) raw_dir else file.path(raw_dir, season)

  games <- espn_nba_season_game_ids(season = season, season_type = season_type)
  if (!is.null(game_ids) && length(game_ids) > 0 && nrow(games) > 0) {
    games <- dplyr::filter(games, game_id %in% as.integer(game_ids))
  }
  if (nrow(games) == 0) {
    return(tibble::tibble(
      game_id = integer(),
      season = integer(),
      season_type = character(),
      file_path = character(),
      status = character(),
      error = character(),
      timestamp = as.POSIXct(character())
    ))
  }

  if (!"season" %in% names(games)) {
    games$season <- as.integer(season)
  }
  if (!"game_date" %in% names(games)) {
    games$game_date <- as.Date(NA)
  }

  dir.create(season_dir, recursive = TRUE, showWarnings = FALSE)

  total <- nrow(games)
  results <- vector("list", total)
  existing_files <- list.files(
    season_dir,
    pattern = sprintf("^summary_%s_.*\\.json$", season),
    full.names = TRUE
  )
  existing_ids <- .espn_nba_parse_game_id_from_filename(existing_files)
  existing_map <- stats::setNames(existing_files, existing_ids)

  if (isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
    on.exit(close(pb), add = TRUE)
  }

  for (row in seq_len(total)) {
    game_id <- as.integer(games$game_id[[row]])
    season_value <- as.integer(games$season[[row]])
    game_date <- games$game_date[[row]]
    game_date_key <- .espn_nba_clean_date_key(game_date)
    key <- .espn_nba_summary_key(
      season = season_value,
      game_date = game_date_key,
      game_id = game_id
    )
    expected_path <- file.path(season_dir, paste0("summary_", key, ".json"))
    path <- expected_path

    status <- "saved"
    error_msg <- NA_character_
    status_detail <- NA_character_

    if (!isTRUE(overwrite) && !is.na(existing_map[as.character(game_id)])) {
      status <- "skipped_exists"
      path <- existing_map[as.character(game_id)]
    } else {
      fetch_out <- tryCatch(
        fetch_save_fn(game_id = game_id, raw_dir = season_dir, file_path = path),
        error = function(e) list(error = conditionMessage(e))
      )
      if (pause_sec > 0) Sys.sleep(pause_sec)
      if (is.list(fetch_out) && "error" %in% names(fetch_out) && !is.null(fetch_out$error)) {
        status <- "failed"
        error_msg <- as.character(fetch_out$error)
      } else if (!file.exists(path)) {
        alt <- list.files(
          season_dir,
          pattern = sprintf("^summary_%s_.*_%s\\.json$", season_value, game_id),
          full.names = TRUE
        )
        if (length(alt) > 0) {
          path <- alt[[1]]
          status_detail <- "saved_unexpected_path"
          existing_map[as.character(game_id)] <- path
        } else {
          status <- "failed"
          error_msg <- "file_not_written"
        }
      } else {
        existing_map[as.character(game_id)] <- path
      }
    }

    results[[row]] <- tibble::tibble(
      game_id = game_id,
      season = season_value,
      season_type = season_type,
      file_path = path,
      status = status,
      status_detail = status_detail,
      error = error_msg,
      timestamp = Sys.time()
    )

    if (isTRUE(progress)) {
      utils::setTxtProgressBar(pb, row)
    } else if (!isTRUE(quiet)) {
      message(sprintf("[%s/%s] %s %s", row, total, status, game_id))
    }
  }

  out <- dplyr::bind_rows(results)
  out
}

#' Collect raw summaries across multiple seasons
#'
#' @param seasons Integer vector of seasons
#' @param season_type One of "regular" or "postseason"
#' @param raw_dir Directory to store raw JSON
#' @param overwrite Overwrite existing raw files
#' @param progress Show a progress bar while collecting
#' @param pause_sec Seconds to wait after each request
#' @return A list with per-season results and a combined tibble
#' @export
collect_raw_seasons <- function(seasons,
                                season_type = c("regular", "postseason"),
                                raw_dir = "data/raw",
                                overwrite = FALSE,
                                progress = TRUE,
                                pause_sec = 0.5) {
  season_type <- match.arg(season_type)
  seasons <- as.integer(seasons)

  results <- lapply(seasons, function(season) {
    collect_raw_season(
      season = season,
      season_type = season_type,
      raw_dir = raw_dir,
      overwrite = overwrite,
      progress = progress,
      pause_sec = pause_sec
    )
  })
  names(results) <- as.character(seasons)

  combined <- dplyr::bind_rows(results, .id = "season_label")
  list(season_results = results, combined = combined)
}
