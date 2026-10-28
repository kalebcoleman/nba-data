# ESPN NBA endpoint wrappers

#' Get ESPN NBA game summary raw JSON
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A list containing the raw summary JSON
#' @export
espn_nba_summary_raw <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  result <- espn_nba_summary_raw_safe(game_id, save_raw = save_raw, raw_dir = raw_dir)
  if (!is.null(result$error)) {
    stop(
      sprintf(
        "ESPN summary request failed (status %s): %s",
        result$status_code,
        result$error
      ),
      call. = FALSE
    )
  }
  result$raw
}

#' Get ESPN NBA game summary raw JSON with status metadata
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @param times Number of retry attempts
#' @param pause_base Base seconds for exponential backoff
#' @param pause_cap Max seconds for exponential backoff
#' @return A list containing status_code, raw, and error
#' @export
espn_nba_summary_raw_safe <- function(game_id,
                                      save_raw = FALSE,
                                      raw_dir = "data/raw",
                                      times = 5,
                                      pause_base = 1,
                                      pause_cap = 60) {
  res <- .espn_nba_summary_response_raw(
    game_id,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap
  )
  status_code <- httr::status_code(res)
  if (is.na(status_code) || status_code < 200 || status_code >= 300) {
    message <- httr::http_status(res)$message
    return(list(status_code = status_code, raw = NULL, error = message))
  }

  raw_summary <- tryCatch(
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")),
    error = function(e) e
  )
  if (inherits(raw_summary, "error")) {
    return(list(status_code = status_code, raw = NULL, error = conditionMessage(raw_summary)))
  }

  if (isTRUE(save_raw)) {
    season <- .espn_nba_extract_summary_season(raw_summary)
    game_date <- .espn_nba_clean_date_key(.espn_nba_extract_summary_date(raw_summary))
    key <- .espn_nba_summary_key(season = season, game_date = game_date, game_id = game_id)
    save_raw_json(raw_summary, "summary", key, dir = raw_dir)
  }

  list(status_code = status_code, raw = raw_summary, error = NULL)
}

#' Check if a summary payload has boxscore data
#'
#' @param raw_summary Summary JSON list
#' @return TRUE if team and player stats are present
#' @export
espn_nba_summary_has_boxscore <- function(raw_summary) {
  has_team_stats <- !is.null(raw_summary$boxscore$teams) &&
    length(raw_summary$boxscore$teams) == 2 &&
    length(raw_summary$boxscore$teams[[1]]$statistics) > 0

  has_player_stats <- !is.null(raw_summary$boxscore$players) &&
    length(raw_summary$boxscore$players) > 0 &&
    length(raw_summary$boxscore$players[[1]]$statistics) > 0

  isTRUE(has_team_stats && has_player_stats)
}

#' Check if a summary payload has betting data
#'
#' @param raw_summary Summary JSON list
#' @return TRUE if betting data is present
#' @export
espn_nba_summary_has_betting <- function(raw_summary) {
  has_pickcenter <- "pickcenter" %in% names(raw_summary)
  has_ats <- "againstTheSpread" %in% names(raw_summary)
  has_predictor <- "predictor" %in% names(raw_summary)
  isTRUE(has_pickcenter || has_ats || has_predictor)
}


#' Get ESPN NBA scoreboard
#'
#' @param dates Date(s) in YYYYMMDD format
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A data frame of games
#' @export
espn_nba_scoreboard <- function(dates, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  dates <- as.character(dates)
  resp <- .espn_nba_scoreboard_response(dates)
  raw_sched <- jsonlite::fromJSON(
    resp,
    simplifyDataFrame = FALSE,
    simplifyVector = FALSE,
    simplifyMatrix = FALSE
  )
  if (isTRUE(save_raw)) {
    save_raw_json(raw_sched, "scoreboard", dates, dir = raw_dir)
  }

  events <- raw_sched[["events"]]
  if (length(events) == 0) {
    return(data.frame())
  }

  purrr::map_dfr(events, .espn_nba_parse_scoreboard_event)
}

.espn_nba_parse_scoreboard_event <- function(event) {
  competition <- event$competitions[[1]]
  date_str <- competition$date
  date_clean <- sub("Z$", "", date_str)
  game_date_time <- suppressWarnings(lubridate::ymd_hms(date_clean))
  if (is.na(game_date_time)) {
    game_date_time <- suppressWarnings(lubridate::ymd_hm(date_clean))
  }
  game_date_time <- lubridate::with_tz(game_date_time, tzone = "America/New_York")
  game_date <- as.Date(substr(game_date_time, 1, 10))

  season <- event$season$year
  season_type <- event$season$type
  status_type <- competition$status$type
  status_state <- if (!is.null(status_type$state)) as.character(status_type$state) else NA_character_
  status_name <- if (!is.null(status_type$name)) as.character(status_type$name) else NA_character_
  status_completed <- isTRUE(status_type$completed) || identical(status_state, "post")

  competitors <- competition$competitors
  home_idx <- which(vapply(competitors, function(x) x$homeAway, "") == "home")
  away_idx <- which(vapply(competitors, function(x) x$homeAway, "") == "away")
  if (length(home_idx) == 0) home_idx <- 1
  if (length(away_idx) == 0) away_idx <- 2

  home <- competitors[[home_idx[1]]]
  away <- competitors[[away_idx[1]]]

  data.frame(
    game_id = as.integer(competition$id),
    season = as.integer(season),
    season_type = as.integer(season_type),
    game_date = game_date,
    game_date_time = game_date_time,
    home_team_id = as.integer(home$team$id),
    away_team_id = as.integer(away$team$id),
    home_team_abbreviation = .espn_nba_null_chr(home$team$abbreviation),
    away_team_abbreviation = .espn_nba_null_chr(away$team$abbreviation),
    status_completed = status_completed,
    status_name = status_name,
    status_state = status_state,
    stringsAsFactors = FALSE
  )
}

.espn_nba_default_season_window <- function(season, season_type) {
  season <- as.integer(season)
  season_type <- match.arg(season_type, c("regular", "postseason", "all"))

  if (season_type == "regular") {
    start_date <- as.Date(paste0(season - 1, "-10-15"))
    end_date <- as.Date(paste0(season, "-04-20"))
  } else if (season_type == "postseason") {
    start_date <- as.Date(paste0(season, "-04-10"))
    end_date <- as.Date(paste0(season, "-06-30"))
  } else {
    start_date <- as.Date(paste0(season - 1, "-10-15"))
    end_date <- as.Date(paste0(season, "-06-30"))
  }

  list(start_date = start_date, end_date = end_date)
}

.espn_nba_season_date_range <- function(season, season_type, start_date = NULL, end_date = NULL) {
  defaults <- .espn_nba_default_season_window(season, season_type)

  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
  } else {
    start_date <- defaults$start_date
  }

  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
  } else {
    end_date <- defaults$end_date
  }

  seq.Date(start_date, end_date, by = "day")
}

.espn_nba_null_chr <- function(x) {
  if (is.null(x)) NA_character_ else as.character(x)
}

#' Get the ESPN NBA schedule for a season
#'
#' Returns every game in the date window, including games that have not been
#' played yet (`status_state == "pre"`). ESPN's scoreboard endpoint only
#' accepts single dates, so this makes one request per day.
#'
#' @param season Season year (the year the season ends, e.g. 2027 for 2026-27)
#' @param season_type One of "regular", "postseason", or "all"
#' @param start_date Optional start date (defaults to the season window)
#' @param end_date Optional end date (defaults to the season window)
#' @param pause_sec Seconds to wait between requests
#' @param scoreboard_fn Function that returns games for one YYYYMMDD date
#' @return A data frame with one row per game
#' @export
espn_nba_schedule <- function(season,
                              season_type = c("regular", "postseason", "all"),
                              start_date = NULL,
                              end_date = NULL,
                              pause_sec = 0.2,
                              scoreboard_fn = espn_nba_scoreboard) {
  season_type <- match.arg(season_type)

  dates <- .espn_nba_season_date_range(season, season_type, start_date, end_date)
  if (length(dates) == 0) {
    return(data.frame())
  }

  games <- lapply(dates, function(x) {
    out <- scoreboard_fn(format(x, "%Y%m%d"))
    if (pause_sec > 0) Sys.sleep(pause_sec)
    out
  })
  games <- dplyr::bind_rows(games)

  if (nrow(games) == 0) {
    return(games)
  }

  games <- games[games$season == as.integer(season), , drop = FALSE]
  if (season_type == "regular") {
    games <- games[games$season_type == 2, , drop = FALSE]
  } else if (season_type == "postseason") {
    games <- games[games$season_type == 3, , drop = FALSE]
  }

  games <- games[!duplicated(games$game_id), , drop = FALSE]
  games[order(games$game_date_time, games$game_id), , drop = FALSE]
}

#' Get ESPN NBA season game ids
#'
#' Completed games from [espn_nba_schedule()].
#'
#' @inheritParams espn_nba_schedule
#' @return A data frame of completed games with ids
#' @export
espn_nba_season_game_ids <- function(season,
                                     season_type = c("regular", "postseason", "all"),
                                     start_date = NULL,
                                     end_date = NULL,
                                     pause_sec = 0.2) {
  games <- espn_nba_schedule(season, season_type, start_date, end_date, pause_sec = pause_sec)
  if (nrow(games) == 0) {
    return(games)
  }
  games[games$status_completed %in% TRUE, , drop = FALSE]
}



#' Get ESPN NBA team box scores
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A team boxscore data frame
#' @export
espn_nba_team_box <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  resp <- .espn_nba_summary_response(game_id)
  if (isTRUE(save_raw)) {
    save_raw_json(jsonlite::fromJSON(resp), "summary", game_id, dir = raw_dir)
  }

  team_box_score <- NULL
  tryCatch(
    expr = {
      team_box_score <- helper_espn_nba_team_box(resp)
      if (is.null(team_box_score)) {
        message(glue::glue("{Sys.time()}: No team box score data for {game_id} available!"))
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no team box score data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  if (is.null(team_box_score)) {
    raw_summary <- tryCatch(
      jsonlite::fromJSON(resp, simplifyDataFrame = FALSE),
      error = function(e) NULL
    )
    if (!is.null(raw_summary$boxscore$teams)) {
      team_box_score <- jsonlite::fromJSON(
        jsonlite::toJSON(raw_summary$boxscore$teams),
        flatten = TRUE
      )
      team_box_score <- janitor::clean_names(team_box_score)
      team_box_score$game_id <- as.integer(game_id)
    }
  }

  team_box_score
}

#' Get ESPN NBA player box scores
#'
#' @param game_id Game ID
#' @param save_raw Save raw JSON to disk
#' @param raw_dir Directory to store raw JSON
#' @return A player boxscore data frame
#' @export
espn_nba_player_box <- function(game_id, save_raw = FALSE, raw_dir = "data/raw") {
  old <- options(list(stringsAsFactors = FALSE, scipen = 999))
  on.exit(options(old))

  resp <- .espn_nba_summary_response(game_id)
  if (isTRUE(save_raw)) {
    save_raw_json(jsonlite::fromJSON(resp), "summary", game_id, dir = raw_dir)
  }

  player_box_score <- NULL
  tryCatch(
    expr = {
      player_box_score <- helper_espn_nba_player_box(resp)
      if (is.null(player_box_score)) {
        message(glue::glue("{Sys.time()}: No player box score data for {game_id} available!"))
      }
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: Invalid arguments or no player box score data for {game_id} available!"))
    },
    warning = function(w) {
    },
    finally = {
    }
  )

  if (is.null(player_box_score)) {
    raw_summary <- tryCatch(
      jsonlite::fromJSON(resp, simplifyDataFrame = FALSE),
      error = function(e) NULL
    )
    if (!is.null(raw_summary$boxscore$players)) {
      player_box_score <- jsonlite::fromJSON(
        jsonlite::toJSON(raw_summary$boxscore$players),
        flatten = TRUE
      )
      player_box_score <- janitor::clean_names(player_box_score)
      player_box_score$game_id <- as.integer(game_id)
    }
  }

  player_box_score
}
