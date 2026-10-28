#' Parse ESPN NBA game summary metadata from raw JSON
#'
#' @param raw_summary Summary JSON list
#' @return A tibble with one row per game
#' @export
espn_nba_games_from_raw <- function(raw_summary) {
  if (is.null(raw_summary$header)) {
    return(.espn_nba_empty_table("games"))
  }

  header <- raw_summary$header
  competition <- header$competitions
  if (is.list(competition) && length(competition) > 0 && !is.null(competition[[1]]$date)) {
    competition <- competition[[1]]
  }

  date_str <- competition$date
  game_date_time <- as.POSIXct(NA)
  if (!is.null(date_str)) {
    date_clean <- sub("Z$", "", as.character(date_str))
    game_date_time <- suppressWarnings(lubridate::ymd_hms(date_clean))
    if (is.na(game_date_time)) {
      game_date_time <- suppressWarnings(lubridate::ymd_hm(date_clean))
    }
    if (!is.na(game_date_time)) {
      game_date_time <- lubridate::with_tz(game_date_time, tzone = "America/New_York")
    }
  }
  game_date <- if (!is.na(game_date_time)) {
    as.Date(substr(game_date_time, 1, 10))
  } else {
    as.Date(NA)
  }

  status_type <- competition$status$type
  status_state <- if (!is.null(status_type$state)) as.character(status_type$state) else NA_character_
  status_name <- if (!is.null(status_type$name)) as.character(status_type$name) else NA_character_
  status_completed <- isTRUE(status_type$completed) || identical(status_state, "post")
  if (status_name %in% c("STATUS_POSTPONED", "STATUS_CANCELED")) {
    status_completed <- FALSE
  }

  competitors <- competition$competitors
  if (!is.null(competitors) && is.data.frame(competitors)) {
    competitors <- lapply(seq_len(nrow(competitors)), function(i) {
      as.list(competitors[i, , drop = FALSE])
    })
  }
  home_away <- vapply(competitors, function(x) {
    val <- x$homeAway
    if (is.list(val)) {
      val <- val[[1]]
    }
    if (length(val) == 0 || is.null(val)) {
      return("")
    }
    as.character(val)[1]
  }, character(1))
  home_idx <- which(home_away == "home")
  away_idx <- which(home_away == "away")
  if (length(home_idx) == 0) home_idx <- 1
  if (length(away_idx) == 0) away_idx <- 2

  home <- competitors[[home_idx[1]]]
  away <- competitors[[away_idx[1]]]

  home_team_id <- if (!is.null(home$team$id)) home$team$id else home$id
  away_team_id <- if (!is.null(away$team$id)) away$team$id else away$id

  games <- tibble::tibble(
    game_id = as.integer(header$id),
    season = as.integer(header$season$year),
    season_type = as.integer(header$season$type),
    game_date = game_date,
    game_date_time = game_date_time,
    status_completed = status_completed,
    status_name = status_name,
    status_state = status_state,
    home_team_id = as.integer(home_team_id),
    away_team_id = as.integer(away_team_id),
    home_team_score = as.integer(home$score),
    away_team_score = as.integer(away$score),
    home_team_winner = isTRUE(home$winner),
    away_team_winner = isTRUE(away$winner)
  )

  .espn_nba_apply_schema(games, "games")
}

#' Parse ESPN NBA team box score from raw JSON
#'
#' @param raw_summary Summary JSON list
#' @return A data frame of team box scores
#' @export
espn_nba_team_box_from_raw <- function(raw_summary) {
  teams <- raw_summary$boxscore$teams
  if (is.null(teams) || length(teams) < 2) {
    return(.espn_nba_empty_table("team_box"))
  }
  if (length(teams[[1]]$statistics) == 0) {
    return(.espn_nba_empty_table("team_box"))
  }

  resp <- jsonlite::toJSON(raw_summary, auto_unbox = TRUE, null = "null")
  team_box <- tryCatch(
    helper_espn_nba_team_box(resp),
    error = function(e) NULL
  )
  if (is.null(team_box)) {
    return(.espn_nba_empty_table("team_box"))
  }

  .espn_nba_apply_schema(team_box, "team_box")
}

#' Parse ESPN NBA player box score from raw JSON
#'
#' @param raw_summary Summary JSON list
#' @return A data frame of player box scores
#' @export
espn_nba_player_box_from_raw <- function(raw_summary) {
  players <- raw_summary$boxscore$players
  if (is.null(players) || length(players) < 1) {
    return(.espn_nba_empty_table("player_box"))
  }
  stats <- players[[1]]$statistics
  if (is.null(stats) || length(stats) == 0) {
    return(.espn_nba_empty_table("player_box"))
  }

  resp <- jsonlite::toJSON(raw_summary, auto_unbox = TRUE, null = "null")
  player_box <- tryCatch(
    helper_espn_nba_player_box(resp),
    error = function(e) NULL
  )
  if (is.null(player_box)) {
    return(.espn_nba_empty_table("player_box"))
  }

  .espn_nba_apply_schema(player_box, "player_box")
}

.espn_nba_schema_names <- function(table) {
  names(.espn_nba_schema()[[table]])
}

.espn_nba_schema <- function() {
  list(
    games = c(
      game_id = "integer",
      season = "integer",
      season_type = "integer",
      game_date = "date",
      game_date_time = "posixct",
      status_completed = "logical",
      status_name = "character",
      status_state = "character",
      home_team_id = "integer",
      away_team_id = "integer",
      home_team_score = "integer",
      away_team_score = "integer",
      home_team_winner = "logical",
      away_team_winner = "logical"
    ),
    team_box = c(
      game_id = "integer",
      season = "integer",
      season_type = "integer",
      game_date = "date",
      game_date_time = "posixct",
      team_id = "integer",
      team_uid = "character",
      team_slug = "character",
      team_location = "character",
      team_name = "character",
      team_abbreviation = "character",
      team_display_name = "character",
      team_short_display_name = "character",
      team_color = "character",
      team_alternate_color = "character",
      team_logo = "character",
      team_home_away = "character",
      team_score = "integer",
      team_winner = "logical",
      assists = "integer",
      blocks = "integer",
      defensive_rebounds = "integer",
      fast_break_points = "integer",
      field_goal_pct = "numeric",
      field_goals_made = "integer",
      field_goals_attempted = "integer",
      flagrant_fouls = "integer",
      fouls = "integer",
      free_throw_pct = "numeric",
      free_throws_made = "integer",
      free_throws_attempted = "integer",
      largest_lead = "integer",
      offensive_rebounds = "integer",
      points_in_paint = "integer",
      steals = "integer",
      team_turnovers = "integer",
      technical_fouls = "integer",
      three_point_field_goal_pct = "numeric",
      three_point_field_goals_made = "integer",
      three_point_field_goals_attempted = "integer",
      total_rebounds = "integer",
      total_technical_fouls = "integer",
      total_turnovers = "integer",
      turnover_points = "integer",
      turnovers = "integer",
      opponent_team_id = "integer",
      opponent_team_uid = "character",
      opponent_team_slug = "character",
      opponent_team_location = "character",
      opponent_team_name = "character",
      opponent_team_abbreviation = "character",
      opponent_team_display_name = "character",
      opponent_team_short_display_name = "character",
      opponent_team_color = "character",
      opponent_team_alternate_color = "character",
      opponent_team_logo = "character",
      opponent_team_score = "integer",
      lead_changes = "integer",
      lead_percentage = "numeric"
    ),
    player_box = c(
      game_id = "integer",
      season = "integer",
      season_type = "integer",
      game_date = "date",
      game_date_time = "posixct",
      athlete_id = "integer",
      athlete_display_name = "character",
      team_id = "integer",
      team_name = "character",
      team_location = "character",
      team_short_display_name = "character",
      minutes = "character",
      field_goals_made = "integer",
      field_goals_attempted = "integer",
      three_point_field_goals_made = "integer",
      three_point_field_goals_attempted = "integer",
      free_throws_made = "integer",
      free_throws_attempted = "integer",
      offensive_rebounds = "integer",
      defensive_rebounds = "integer",
      rebounds = "integer",
      assists = "integer",
      steals = "integer",
      blocks = "integer",
      turnovers = "integer",
      fouls = "integer",
      plus_minus = "character",
      points = "integer",
      starter = "logical",
      ejected = "logical",
      did_not_play = "logical",
      reason = "character",
      active = "logical",
      athlete_jersey = "character",
      athlete_short_name = "character",
      athlete_headshot_href = "character",
      athlete_position_name = "character",
      athlete_position_abbreviation = "character",
      team_display_name = "character",
      team_uid = "character",
      team_slug = "character",
      team_logo = "character",
      team_abbreviation = "character",
      team_color = "character",
      team_alternate_color = "character",
      home_away = "character",
      team_winner = "logical",
      team_score = "integer",
      opponent_team_id = "integer",
      opponent_team_name = "character",
      opponent_team_location = "character",
      opponent_team_display_name = "character",
      opponent_team_abbreviation = "character",
      opponent_team_logo = "character",
      opponent_team_color = "character",
      opponent_team_alternate_color = "character",
      opponent_team_score = "integer"
    )
  )
}

.espn_nba_empty_value <- function(type, n = 0) {
  switch(
    type,
    integer = if (n == 0) integer() else rep(NA_integer_, n),
    numeric = if (n == 0) numeric() else rep(NA_real_, n),
    character = if (n == 0) character() else rep(NA_character_, n),
    logical = if (n == 0) logical() else rep(NA, n),
    date = if (n == 0) as.Date(character()) else as.Date(rep(NA_character_, n)),
    posixct = if (n == 0) as.POSIXct(character()) else as.POSIXct(rep(NA_character_, n)),
    if (n == 0) logical() else rep(NA, n)
  )
}

.espn_nba_empty_table <- function(table) {
  schema <- .espn_nba_schema()[[table]]
  if (is.null(schema)) {
    return(tibble::tibble())
  }

  cols <- lapply(schema, .espn_nba_empty_value, n = 0)
  names(cols) <- names(schema)
  tibble::tibble(!!!cols)
}

.espn_nba_apply_schema <- function(df, table) {
  schema <- .espn_nba_schema()[[table]]
  if (is.null(schema)) {
    return(df)
  }

  missing <- setdiff(names(schema), names(df))
  if (length(missing) > 0) {
    n <- nrow(df)
    for (col in missing) {
      df[[col]] <- .espn_nba_empty_value(schema[[col]], n = n)
    }
  }

  for (col in names(schema)) {
    df[[col]] <- .espn_nba_cast_column(df[[col]], schema[[col]])
  }

  ordered <- c(names(schema), setdiff(names(df), names(schema)))
  df[, ordered, drop = FALSE]
}

.espn_nba_cast_column <- function(x, type) {
  if (is.null(x)) {
    return(.espn_nba_empty_value(type, n = 0))
  }
  if (type == "integer") {
    return(as.integer(x))
  }
  if (type == "numeric") {
    return(as.numeric(x))
  }
  if (type == "character") {
    return(as.character(x))
  }
  if (type == "logical") {
    return(as.logical(x))
  }
  if (type == "date") {
    return(as.Date(x))
  }
  if (type == "posixct") {
    return(as.POSIXct(x, tz = "America/New_York"))
  }
  x
}

#' Parse raw summary files for a season
#'
#' @param season Season year used to filter summary files
#' @param raw_dir Directory containing raw JSON
#' @param progress Show a progress bar while parsing
#' @param files Optional vector of file paths to parse instead of the whole
#'   season directory
#' @return Named list with games, team_box, player_box, and file_index
#' @export
espn_nba_parse_raw_dir <- function(season, raw_dir = "data/raw", progress = TRUE, files = NULL) {
  if (is.null(files)) {
    season <- as.character(season)
    pattern <- sprintf("^summary_%s_.*\\.json$", season)
    season_dir <- file.path(raw_dir, season)
    source_dir <- if (dir.exists(season_dir)) season_dir else raw_dir
    files <- list.files(source_dir, pattern = pattern, full.names = TRUE)
  }
  if (length(files) == 0) {
    return(list(
      games = .espn_nba_empty_table("games"),
      team_box = .espn_nba_empty_table("team_box"),
      player_box = .espn_nba_empty_table("player_box"),
      file_index = tibble::tibble(file_path = character())
    ))
  }

  parse_file <- function(path) {
    raw <- tryCatch(
      jsonlite::fromJSON(path, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(raw)) {
      return(list(
        games = .espn_nba_empty_table("games"),
        team_box = .espn_nba_empty_table("team_box"),
        player_box = .espn_nba_empty_table("player_box")
      ))
    }

    list(
      games = espn_nba_games_from_raw(raw),
      team_box = espn_nba_team_box_from_raw(raw),
      player_box = espn_nba_player_box_from_raw(raw)
    )
  }

  if (isTRUE(progress)) {
    pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    on.exit(close(pb), add = TRUE)
    results <- lapply(seq_along(files), function(i) {
      out <- parse_file(files[[i]])
      utils::setTxtProgressBar(pb, i)
      out
    })
  } else {
    results <- lapply(files, parse_file)
  }
  games <- dplyr::bind_rows(lapply(results, `[[`, "games"))
  team_box <- dplyr::bind_rows(lapply(results, `[[`, "team_box"))
  player_box <- dplyr::bind_rows(lapply(results, `[[`, "player_box"))

  list(
    games = games,
    team_box = team_box,
    player_box = player_box,
    file_index = tibble::tibble(file_path = files)
  )
}
