# Helper utilities


check_status <- function(res) {
  status <- httr::status_code(res)
  if (is.na(status) || status < 200 || status >= 300) {
    stop(sprintf("ESPN request failed with status %s", status), call. = FALSE)
  }
}

.espn_nba_request <- function(url, times = 5, pause_base = 1, pause_cap = 60) {
  httr::RETRY(
    "GET",
    url,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap
  )
}

.espn_nba_get <- function(url, times = 5, pause_base = 1, pause_cap = 60) {
  res <- .espn_nba_request(url, times = times, pause_base = pause_base, pause_cap = pause_cap)
  check_status(res)
  httr::content(res, as = "text", encoding = "UTF-8")
}

.espn_nba_summary_response_raw <- function(game_id, times = 5, pause_base = 1, pause_cap = 60) {
  summary_url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/summary?"
  full_url <- paste0(summary_url, "event=", game_id)

  .espn_nba_request(full_url, times = times, pause_base = pause_base, pause_cap = pause_cap)
}

.espn_nba_summary_response <- function(game_id, times = 5, pause_base = 1, pause_cap = 60) {
  res <- .espn_nba_summary_response_raw(
    game_id,
    times = times,
    pause_base = pause_base,
    pause_cap = pause_cap
  )
  check_status(res)
  httr::content(res, as = "text", encoding = "UTF-8")
}

.espn_nba_scoreboard_response <- function(dates) {
  scoreboard_url <- glue::glue(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/nba/scoreboard?limit=1000&dates={dates}"
  )

  .espn_nba_get(scoreboard_url)
}


.espn_nba_editorial_fields <- c("news", "article", "videos")

save_raw_json <- function(raw, endpoint, key, dir = "data/raw") {
  if (is.null(raw)) {
    return(invisible(NULL))
  }

  # Editorial content (news recaps, articles, video links) is not used by the
  # parsers and is not ours to store, so drop it before writing.
  if (is.list(raw)) {
    raw[intersect(names(raw), .espn_nba_editorial_fields)] <- NULL
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  safe_key <- gsub("[^A-Za-z0-9_-]+", "_", as.character(key))
  path <- file.path(dir, paste0(endpoint, "_", safe_key, ".json"))

  jsonlite::write_json(raw, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

.espn_nba_clean_date_key <- function(date_value) {
  if (is.null(date_value)) {
    return(NULL)
  }

  if (inherits(date_value, "Date")) {
    return(format(date_value, "%Y%m%d"))
  }

  date_str <- as.character(date_value)
  date_str <- sub("Z$", "", date_str)
  date_str <- substr(date_str, 1, 10)
  date_str <- gsub("-", "", date_str)

  if (!nzchar(date_str)) {
    return(NULL)
  }

  date_str
}

.espn_nba_summary_key <- function(season = NULL, game_date = NULL, game_id = NULL) {
  parts <- c(as.character(season), as.character(game_date), as.character(game_id))
  parts <- parts[!is.na(parts) & nzchar(parts)]
  if (length(parts) == 0) {
    return(as.character(game_id))
  }
  paste(parts, collapse = "_")
}

.espn_nba_extract_summary_season <- function(raw_summary) {
  season <- raw_summary$season$year
  if (is.null(season)) {
    season <- raw_summary$header$season$year
  }
  if (is.list(season)) {
    season <- season[[1]]
  } else if (length(season) > 1) {
    season <- season[1]
  }
  if (is.null(season) || is.na(season)) {
    return(NULL)
  }
  as.character(season)
}

.espn_nba_extract_summary_date <- function(raw_summary) {
  date_value <- raw_summary$header$competitions$date
  if (is.null(date_value)) {
    return(NULL)
  }
  if (is.list(date_value)) {
    date_value <- date_value[[1]]
  } else if (length(date_value) > 1) {
    date_value <- date_value[1]
  }
  date_value
}

log_failed_scrape <- function(game_id,
                              game_date,
                              status_code,
                              error_message,
                              dir = "data/raw",
                              filename = "scrape_failures.csv") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, filename)
  entry <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    game_id = as.integer(game_id),
    game_date = as.character(game_date),
    status_code = as.integer(status_code),
    error_message = as.character(error_message),
    stringsAsFactors = FALSE
  )

  utils::write.table(
    entry,
    path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(path),
    append = file.exists(path)
  )
  invisible(path)
}

#' Parse ESPN NBA Team Box
#'
#' @param resp Response object from the ESPN NBA game summary endpoint
#' @return Returns a data frame
#' @export
helper_espn_nba_team_box <- function(resp) {
  game_json <- resp %>%
    jsonlite::fromJSON()

  game_id <- as.integer(game_json[["header"]][["id"]])
  game_date_time <- substr(
    game_json[["header"]][["competitions"]][["date"]],
    1,
    nchar(game_json[["header"]][["competitions"]][["date"]]) - 1
  ) %>%
    lubridate::ymd_hm() %>%
    lubridate::with_tz(tzone = "America/New_York")

  game_date <- as.Date(substr(game_date_time, 0, 10))
  box_score_available <- game_json[["header"]][["competitions"]][["boxscoreAvailable"]]

  if (box_score_available == TRUE) {
    teams_box_score_df <- game_json[["boxscore"]][["teams"]] %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON(flatten = TRUE)

    if (length(teams_box_score_df[["statistics"]][[1]]) > 0) {
      teams_df <- game_json[["header"]][["competitions"]][["competitors"]][[1]]

      home_away_1 <- teams_df[["homeAway"]][1]
      home_away_1_team_id <- as.integer(teams_df[["id"]][1])
      home_away_1_team_score <- as.integer(teams_df[["score"]][1])
      home_away_1_team_winner <- teams_df[["winner"]][1]

      home_away_2 <- teams_df[["homeAway"]][2]
      home_away_2_team_id <- as.integer(teams_df[["id"]][2])
      home_away_2_team_score <- as.integer(teams_df[["score"]][2])
      home_away_2_team_winner <- teams_df[["winner"]][2]

      statistics_df_1 <- teams_box_score_df[["statistics"]][[1]] %>%
        tibble::tibble() %>%
        dplyr::select("name", "displayValue") %>%
        tidyr::spread("name", "displayValue")

      statistics_df_2 <- teams_box_score_df[["statistics"]][[2]] %>%
        tibble::tibble() %>%
        dplyr::select("name", "displayValue") %>%
        tidyr::spread("name", "displayValue")

      statistics_df_1$team.homeAway <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(home_away_1_team_id),
        home_away_1,
        home_away_2
      )
      statistics_df_1$team.score <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(home_away_1_team_id),
        as.integer(home_away_1_team_score),
        as.integer(home_away_2_team_score)
      )
      statistics_df_1$team.winner <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(home_away_1_team_id),
        home_away_1_team_winner,
        home_away_2_team_winner
      )
      statistics_df_1$team.id <- as.integer(teams_box_score_df[["team.id"]][[1]])
      statistics_df_1$team.uid <- teams_box_score_df[["team.uid"]][[1]]
      statistics_df_1$team.slug <- teams_box_score_df[["team.slug"]][[1]]
      statistics_df_1$team.location <- teams_box_score_df[["team.location"]][[1]]
      statistics_df_1$team.name <- teams_box_score_df[["team.name"]][[1]]
      statistics_df_1$team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[1]]
      statistics_df_1$team.displayName <- teams_box_score_df[["team.displayName"]][[1]]
      statistics_df_1$team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[1]]
      statistics_df_1$team.color <- teams_box_score_df[["team.color"]][[1]]
      statistics_df_1$team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[1]]
      statistics_df_1$team.logo <- teams_box_score_df[["team.logo"]][[1]]
      statistics_df_1$opponent.team.id <- as.integer(teams_box_score_df[["team.id"]][[2]])
      statistics_df_1$opponent.team.uid <- teams_box_score_df[["team.uid"]][[2]]
      statistics_df_1$opponent.team.slug <- teams_box_score_df[["team.slug"]][[2]]
      statistics_df_1$opponent.team.location <- teams_box_score_df[["team.location"]][[2]]
      statistics_df_1$opponent.team.name <- teams_box_score_df[["team.name"]][[2]]
      statistics_df_1$opponent.team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[2]]
      statistics_df_1$opponent.team.displayName <- teams_box_score_df[["team.displayName"]][[2]]
      statistics_df_1$opponent.team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[2]]
      statistics_df_1$opponent.team.color <- teams_box_score_df[["team.color"]][[2]]
      statistics_df_1$opponent.team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[2]]
      statistics_df_1$opponent.team.logo <- teams_box_score_df[["team.logo"]][[2]]
      statistics_df_1$opponent.team.score <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][1]) == as.integer(home_away_1_team_id),
        as.integer(home_away_2_team_score),
        as.integer(home_away_1_team_score)
      )

      statistics_df_2$team.homeAway <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(home_away_2_team_id),
        home_away_2,
        home_away_1
      )
      statistics_df_2$team.score <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(home_away_2_team_id),
        as.integer(home_away_2_team_score),
        as.integer(home_away_1_team_score)
      )
      statistics_df_2$team.winner <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(home_away_2_team_id),
        home_away_2_team_winner,
        home_away_1_team_winner
      )
      statistics_df_2$team.id <- as.integer(teams_box_score_df[["team.id"]][[2]])
      statistics_df_2$team.uid <- teams_box_score_df[["team.uid"]][[2]]
      statistics_df_2$team.slug <- teams_box_score_df[["team.slug"]][[2]]
      statistics_df_2$team.location <- teams_box_score_df[["team.location"]][[2]]
      statistics_df_2$team.name <- teams_box_score_df[["team.name"]][[2]]
      statistics_df_2$team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[2]]
      statistics_df_2$team.displayName <- teams_box_score_df[["team.displayName"]][[2]]
      statistics_df_2$team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[2]]
      statistics_df_2$team.color <- teams_box_score_df[["team.color"]][[2]]
      statistics_df_2$team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[2]]
      statistics_df_2$team.logo <- teams_box_score_df[["team.logo"]][[2]]
      statistics_df_2$opponent.team.id <- as.integer(teams_box_score_df[["team.id"]][[1]])
      statistics_df_2$opponent.team.uid <- teams_box_score_df[["team.uid"]][[1]]
      statistics_df_2$opponent.team.slug <- teams_box_score_df[["team.slug"]][[1]]
      statistics_df_2$opponent.team.location <- teams_box_score_df[["team.location"]][[1]]
      statistics_df_2$opponent.team.name <- teams_box_score_df[["team.name"]][[1]]
      statistics_df_2$opponent.team.abbreviation <- teams_box_score_df[["team.abbreviation"]][[1]]
      statistics_df_2$opponent.team.displayName <- teams_box_score_df[["team.displayName"]][[1]]
      statistics_df_2$opponent.team.shortDisplayName <- teams_box_score_df[["team.shortDisplayName"]][[1]]
      statistics_df_2$opponent.team.color <- teams_box_score_df[["team.color"]][[1]]
      statistics_df_2$opponent.team.alternateColor <- teams_box_score_df[["team.alternateColor"]][[1]]
      statistics_df_2$opponent.team.logo <- teams_box_score_df[["team.logo"]][[1]]
      statistics_df_2$opponent.team.score <- ifelse(
        as.integer(teams_box_score_df[["team.id"]][2]) == as.integer(home_away_2_team_id),
        as.integer(home_away_1_team_score),
        as.integer(home_away_2_team_score)
      )

      complete_statistics_df <- statistics_df_1 %>%
        dplyr::bind_rows(statistics_df_2)

      complete_statistics_df$season <- game_json[["header"]][["season"]][["year"]]
      complete_statistics_df$season_type <- game_json[["header"]][["season"]][["type"]]
      complete_statistics_df$game_id <- as.integer(game_id)
      complete_statistics_df$game_date_time <- game_date_time
      complete_statistics_df$game_date <- game_date

      suppressWarnings(
        complete_statistics_df <- complete_statistics_df %>%
          tidyr::separate(
            "fieldGoalsMade-fieldGoalsAttempted",
            into = c("fieldGoalsMade", "fieldGoalsAttempted"),
            sep = "-"
          ) %>%
          tidyr::separate(
            "freeThrowsMade-freeThrowsAttempted",
            into = c("freeThrowsMade", "freeThrowsAttempted"),
            sep = "-"
          ) %>%
          tidyr::separate(
            "threePointFieldGoalsMade-threePointFieldGoalsAttempted",
            into = c("threePointFieldGoalsMade", "threePointFieldGoalsAttempted"),
            sep = "-"
          ) %>%
          dplyr::mutate(dplyr::across(c(
            "fieldGoalPct",
            "freeThrowPct",
            "threePointFieldGoalPct"
          ), as.numeric)) %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(c(
            "assists",
            "blocks",
            "defensiveRebounds",
            "fieldGoalsMade",
            "fieldGoalsAttempted",
            "flagrantFouls",
            "fouls",
            "freeThrowsMade",
            "freeThrowsAttempted",
            "offensiveRebounds",
            "steals",
            "teamTurnovers",
            "technicalFouls",
            "threePointFieldGoalsMade",
            "threePointFieldGoalsAttempted",
            "totalRebounds",
            "totalTechnicalFouls",
            "totalTurnovers",
            "turnovers"
          )), as.integer))
      )

      team_box_score <- complete_statistics_df %>%
        janitor::clean_names() %>%
        dplyr::select(
          dplyr::any_of(c(
            "game_id",
            "season",
            "season_type",
            "game_date",
            "game_date_time",
            "team_id",
            "team_uid",
            "team_slug",
            "team_location",
            "team_name",
            "team_abbreviation",
            "team_display_name",
            "team_short_display_name",
            "team_color",
            "team_alternate_color",
            "team_logo",
            "team_home_away",
            "team_score",
            "team_winner"
          )),
          tidyr::everything()
        )

      return(team_box_score)
    }
  }
}

#' Parse ESPN NBA Player Box
#'
#' @param resp Response object from the ESPN NBA game summary endpoint
#' @return Returns a data frame
#' @export
helper_espn_nba_player_box <- function(resp) {
  game_json <- resp %>%
    jsonlite::fromJSON(flatten = TRUE)

  players_box_score_df <- game_json[["boxscore"]][["players"]] %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    as.data.frame()

  game_id <- as.integer(game_json[["header"]][["id"]])
  season <- game_json[["header"]][["season"]][["year"]]
  season_type <- game_json[["header"]][["season"]][["type"]]
  game_date_time <- substr(
    game_json[["header"]][["competitions"]][["date"]],
    1,
    nchar(game_json[["header"]][["competitions"]][["date"]]) - 1
  ) %>%
    lubridate::ymd_hm() %>%
    lubridate::with_tz(tzone = "America/New_York")

  game_date <- as.Date(substr(game_date_time, 0, 10))

  box_score_available <- game_json[["header"]][["competitions"]][["boxscoreAvailable"]]

  suppressWarnings(
    valid_stats <- players_box_score_df[["statistics"]][[1]][["athletes"]][[1]][["stats"]][[1]] %>%
      purrr::pluck(7) %>%
      as.numeric()
  )

  if (box_score_available == TRUE &&
      length(players_box_score_df[["statistics"]][[1]][["athletes"]][[1]]) > 1 &&
      !is.na(valid_stats)) {
    players_df <- players_box_score_df %>%
      tidyr::unnest("statistics") %>%
      tidyr::unnest("athletes")

    if (length(players_box_score_df[["statistics"]]) > 1 &&
        length(players_df$stats[[1]]) > 0) {
      players_df <- jsonlite::fromJSON(
        jsonlite::toJSON(game_json[["boxscore"]][["players"]]),
        flatten = TRUE
      ) %>%
        tidyr::unnest("statistics") %>%
        tidyr::unnest("athletes")

      stat_cols <- players_df$keys[[1]]
      stats <- players_df$stats

      stats_df <- as.data.frame(do.call(rbind, stats))
      colnames(stats_df) <- stat_cols

      suppressWarnings(
        stats_df <- stats_df %>%
          tidyr::separate(
            "fieldGoalsMade-fieldGoalsAttempted",
            into = c("fieldGoalsMade", "fieldGoalsAttempted"),
            sep = "-"
          ) %>%
          tidyr::separate(
            "freeThrowsMade-freeThrowsAttempted",
            into = c("freeThrowsMade", "freeThrowsAttempted"),
            sep = "-"
          ) %>%
          tidyr::separate(
            "threePointFieldGoalsMade-threePointFieldGoalsAttempted",
            into = c("threePointFieldGoalsMade", "threePointFieldGoalsAttempted"),
            sep = "-"
          ) %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(c(
            "minutes",
            "fieldGoalPct",
            "freeThrowPct",
            "threePointFieldGoalPct"
          )), as.numeric)) %>%
          dplyr::mutate(dplyr::across(dplyr::any_of(c(
            "assists",
            "blocks",
            "defensiveRebounds",
            "fieldGoalsMade",
            "fieldGoalsAttempted",
            "flagrantFouls",
            "fouls",
            "freeThrowsMade",
            "freeThrowsAttempted",
            "offensiveRebounds",
            "steals",
            "teamTurnovers",
            "technicalFouls",
            "threePointFieldGoalsMade",
            "threePointFieldGoalsAttempted",
            "rebounds",
            "totalTechnicalFouls",
            "totalTurnovers",
            "turnovers",
            "points"
          )), as.integer))
      )

      players_df_did_not_play <- players_df %>%
        dplyr::filter(didNotPlay) %>%
        dplyr::select(dplyr::any_of(c(
          "starter",
          "ejected",
          "didNotPlay",
          "reason",
          "active",
          "athlete.displayName",
          "athlete.jersey",
          "athlete.id",
          "athlete.shortName",
          "athlete.headshot.href",
          "athlete.position.name",
          "athlete.position.abbreviation",
          "team.displayName",
          "team.shortDisplayName",
          "team.location",
          "team.name",
          "team.logo",
          "team.id",
          "team.uid",
          "team.slug",
          "team.abbreviation",
          "team.color",
          "team.alternateColor"
        )))

      players_df <- players_df %>%
        dplyr::filter(!didNotPlay) %>%
        dplyr::select(dplyr::any_of(c(
          "starter",
          "ejected",
          "didNotPlay",
          "reason",
          "active",
          "athlete.displayName",
          "athlete.jersey",
          "athlete.id",
          "athlete.shortName",
          "athlete.headshot.href",
          "athlete.position.name",
          "athlete.position.abbreviation",
          "team.displayName",
          "team.shortDisplayName",
          "team.location",
          "team.name",
          "team.logo",
          "team.id",
          "team.uid",
          "team.slug",
          "team.abbreviation",
          "team.color",
          "team.alternateColor"
        )))

      players_df <- stats_df %>%
        dplyr::bind_cols(players_df) %>%
        dplyr::bind_rows(players_df_did_not_play)

      players_df <- players_df %>%
        dplyr::select(
          dplyr::any_of(c(
            "athlete.displayName",
            "team.shortDisplayName"
          )),
          tidyr::everything()
        ) %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          game_id = game_id,
          season = season,
          season_type = season_type,
          game_date = game_date,
          game_date_time = game_date_time
        )

      teams_df <- game_json[["header"]][["competitions"]][["competitors"]][[1]]

      home_away_1 <- teams_df[["homeAway"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_id <- as.integer(teams_df[["id"]] %>%
        purrr::pluck(1, .default = NA_integer_))
      home_away_1_team_location <- teams_df[["team.location"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_name <- teams_df[["team.name"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_abbreviation <- teams_df[["team.abbreviation"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_display_name <- teams_df[["team.displayName"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_logos <- teams_df[["team.logos"]][[1]][["href"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_color <- teams_df[["team.color"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_alternate_color <- teams_df[["team.alternateColor"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_winner <- teams_df[["winner"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_1_team_score <- as.integer(teams_df[["score"]] %>%
        purrr::pluck(1, .default = NA_integer_))

      home_away_2 <- teams_df[["homeAway"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_id <- as.integer(teams_df[["id"]] %>%
        purrr::pluck(2, .default = NA_integer_))
      home_away_2_team_location <- teams_df[["team.location"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_name <- teams_df[["team.name"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_abbreviation <- teams_df[["team.abbreviation"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_display_name <- teams_df[["team.displayName"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_logos <- teams_df[["team.logos"]][[2]][["href"]] %>%
        purrr::pluck(1, .default = NA_character_)
      home_away_2_team_color <- teams_df[["team.color"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_alternate_color <- teams_df[["team.alternateColor"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_winner <- teams_df[["winner"]] %>%
        purrr::pluck(2, .default = NA_character_)
      home_away_2_team_score <- as.integer(teams_df[["score"]] %>%
        purrr::pluck(2, .default = NA_integer_))

      players_df <- players_df %>%
        dplyr::mutate(
          home_away = ifelse(team_id == home_away_1_team_id, home_away_1, home_away_2),
          team_winner = ifelse(team_id == home_away_1_team_id, home_away_1_team_winner, home_away_2_team_winner),
          team_score = ifelse(team_id == home_away_1_team_id, home_away_1_team_score, home_away_2_team_score),
          opponent_team_id = ifelse(team_id == home_away_1_team_id, home_away_2_team_id, home_away_1_team_id),
          opponent_team_name = ifelse(team_id == home_away_1_team_id, home_away_2_team_name, home_away_1_team_name),
          opponent_team_location = ifelse(team_id == home_away_1_team_id, home_away_2_team_location, home_away_1_team_location),
          opponent_team_display_name = ifelse(team_id == home_away_1_team_id, home_away_2_team_display_name, home_away_1_team_display_name),
          opponent_team_abbreviation = ifelse(team_id == home_away_1_team_id, home_away_2_team_abbreviation, home_away_1_team_abbreviation),
          opponent_team_logo = ifelse(team_id == home_away_1_team_id, home_away_2_team_logos, home_away_1_team_logos),
          opponent_team_color = ifelse(team_id == home_away_1_team_id, home_away_2_team_color, home_away_1_team_color),
          opponent_team_alternate_color = ifelse(team_id == home_away_1_team_id, home_away_2_team_alternate_color, home_away_1_team_alternate_color),
          opponent_team_score = ifelse(team_id == home_away_1_team_id, home_away_2_team_score, home_away_1_team_score)
        ) %>%
        dplyr::arrange(home_away)

      player_box_score <- players_df %>%
        dplyr::select(dplyr::any_of(c(
          "game_id",
          "season",
          "season_type",
          "game_date",
          "game_date_time",
          "athlete_id",
          "athlete_display_name",
          "team_id",
          "team_name",
          "team_location",
          "team_short_display_name",
          "minutes",
          "field_goals_made",
          "field_goals_attempted",
          "three_point_field_goals_made",
          "three_point_field_goals_attempted",
          "free_throws_made",
          "free_throws_attempted",
          "offensive_rebounds",
          "defensive_rebounds",
          "rebounds",
          "assists",
          "steals",
          "blocks",
          "turnovers",
          "fouls",
          "plus_minus",
          "points",
          "starter",
          "ejected",
          "did_not_play",
          "reason",
          "active",
          "athlete_jersey",
          "athlete_short_name",
          "athlete_headshot_href",
          "athlete_position_name",
          "athlete_position_abbreviation",
          "team_display_name",
          "team_uid",
          "team_slug",
          "team_logo",
          "team_abbreviation",
          "team_color",
          "team_alternate_color",
          "home_away",
          "team_winner",
          "team_score",
          "opponent_team_id",
          "opponent_team_name",
          "opponent_team_location",
          "opponent_team_display_name",
          "opponent_team_abbreviation",
          "opponent_team_logo",
          "opponent_team_color",
          "opponent_team_alternate_color",
          "opponent_team_score"
        ))) %>%
        dplyr::mutate_at(c(
          "athlete_id",
          "team_id",
          "team_score",
          "opponent_team_score"
        ), as.integer)

      return(player_box_score)
    }
  }
}
