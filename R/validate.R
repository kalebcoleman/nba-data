#' Validate parsed tables for data quality checks
#'
#' @param tables Named list from espn_nba_parse_raw_dir()
#' @param require_completed_only Only validate completed games when TRUE
#' @param warn_only Emit warnings instead of stopping on issues
#' @return A list with ok, issues, missingness, and samples
#' @export
validate_parsed_tables <- function(tables,
                                   require_completed_only = TRUE,
                                   warn_only = FALSE) {
  games <- tables$games
  team_box <- tables$team_box
  player_box <- tables$player_box

  if (is.null(games)) games <- .espn_nba_empty_table("games")
  if (is.null(team_box)) team_box <- .espn_nba_empty_table("team_box")
  if (is.null(player_box)) player_box <- .espn_nba_empty_table("player_box")

  completed_ids <- unique(games$game_id)
  if (isTRUE(require_completed_only) && "status_completed" %in% names(games)) {
    played <- games$status_completed %in% TRUE
    # ESPN marks some postponed games as completed with a 0-0 score.
    if ("status_name" %in% names(games)) {
      played <- played & !games$status_name %in% .espn_nba_unplayed_status
    }
    completed_ids <- unique(games$game_id[played])
  }
  completed_ids <- suppressWarnings(as.integer(completed_ids))
  completed_ids <- completed_ids[!is.na(completed_ids) & completed_ids > 0]

  issues <- .espn_nba_empty_issues()

  if (length(completed_ids) > 0) {
    team_counts <- dplyr::count(team_box, game_id, name = "team_rows")
    team_counts <- dplyr::filter(team_counts, !is.na(game_id))
    expected <- tibble::tibble(game_id = completed_ids)
    team_counts <- dplyr::left_join(expected, team_counts, by = "game_id")
    team_counts$team_rows[is.na(team_counts$team_rows)] <- 0L
    bad_team <- dplyr::filter(team_counts, team_rows != 2 & !is.na(game_id))
    if (nrow(bad_team) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          bad_team,
          game_id = game_id,
          issue = "team_box_row_count",
          detail = "Expected 2 team rows",
          count = team_rows
        )
      )
    }

    player_counts <- dplyr::count(player_box, game_id, name = "player_rows")
    player_counts <- dplyr::filter(player_counts, !is.na(game_id))
    player_counts <- dplyr::left_join(expected, player_counts, by = "game_id")
    player_counts$player_rows[is.na(player_counts$player_rows)] <- 0L
    bad_player <- dplyr::filter(player_counts, player_rows <= 0 & !is.na(game_id))
    if (nrow(bad_player) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          bad_player,
          game_id = game_id,
          issue = "player_box_row_count",
          detail = "Expected >0 player rows",
          count = player_rows
        )
      )
    }
  }

  if (nrow(team_box) > 0) {
    dup_team <- dplyr::count(team_box, game_id, team_id, name = "dup_count")
    dup_team <- dplyr::filter(dup_team, dup_count > 1)
    if (nrow(dup_team) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          dup_team,
          game_id = game_id,
          issue = "team_box_duplicate",
          detail = "Duplicate (game_id, team_id)",
          count = dup_count
        )
      )
    }
  }

  if (nrow(player_box) > 0) {
    dup_player <- dplyr::count(player_box, game_id, athlete_id, name = "dup_count")
    dup_player <- dplyr::filter(dup_player, dup_count > 1)
    if (nrow(dup_player) > 0) {
      issues <- dplyr::bind_rows(
        issues,
        dplyr::transmute(
          dup_player,
          game_id = game_id,
          issue = "player_box_duplicate",
          detail = "Duplicate (game_id, athlete_id)",
          count = dup_count
        )
      )
    }
  }

  issues$severity <- "error"
  issues <- dplyr::bind_rows(
    issues,
    .espn_nba_value_issues(games, team_box, player_box, completed_ids)
  )
  issues <- dplyr::filter(issues, !is.na(game_id))

  missingness <- list(
    games = .espn_nba_missingness_report(games),
    team_box = .espn_nba_missingness_report(team_box),
    player_box = .espn_nba_missingness_report(player_box)
  )

  samples <- .espn_nba_sample_games(games)

  value_issues <- dplyr::filter(issues, severity == "warning")
  if (nrow(value_issues) > 0) {
    warning(.espn_nba_issue_message("Parsed table value checks found issues", value_issues), call. = FALSE)
  }

  errors <- dplyr::filter(issues, severity == "error")
  ok <- nrow(errors) == 0
  if (!ok) {
    msg <- .espn_nba_issue_message("Parsed table validation failed", errors)

    if (isTRUE(warn_only)) {
      warning(msg, call. = FALSE)
    } else {
      stop(msg, call. = FALSE)
    }
  }

  list(
    ok = ok,
    issues = issues,
    missingness = missingness,
    samples = samples
  )
}

.espn_nba_unplayed_status <- c("STATUS_POSTPONED", "STATUS_CANCELED", "STATUS_SUSPENDED")

.espn_nba_empty_issues <- function() {
  tibble::tibble(
    issue = character(),
    game_id = integer(),
    detail = character(),
    count = integer()
  )
}

.espn_nba_issue_message <- function(prefix, issues) {
  issue_counts <- dplyr::count(issues, issue, name = "n")
  summary <- paste(
    paste0(issue_counts$issue, "=", issue_counts$n),
    collapse = ", "
  )
  sample_rows <- utils::head(issues, 5)
  sample_text <- paste(
    sprintf("%s game_id=%s count=%s", sample_rows$issue, sample_rows$game_id, sample_rows$count),
    collapse = "; "
  )
  sprintf("%s: %s. Sample: %s", prefix, summary, sample_text)
}

# Value-level consistency checks. Each check runs only when its columns are
# present, and failures are reported as warnings: they usually point to errors
# in ESPN's source data rather than in the parser.
.espn_nba_value_issues <- function(games, team_box, player_box, completed_ids) {
  has <- function(df, cols) !is.null(df) && nrow(df) > 0 && all(cols %in% names(df))
  flag <- function(df, issue, detail, count = NA_integer_) {
    if (nrow(df) == 0) {
      return(NULL)
    }
    tibble::tibble(
      issue = issue,
      game_id = as.integer(df$game_id),
      detail = detail,
      count = as.integer(count),
      severity = "warning"
    )
  }
  out <- list()

  played_games <- games
  if (has(games, "game_id")) {
    played_games <- games[as.integer(games$game_id) %in% completed_ids, , drop = FALSE]
  }
  team_box <- team_box[as.integer(team_box$game_id) %in% completed_ids, , drop = FALSE]

  if (has(team_box, "team_home_away")) {
    sides <- dplyr::summarise(
      dplyr::group_by(team_box, game_id),
      n_home = sum(team_home_away == "home", na.rm = TRUE),
      n_away = sum(team_home_away == "away", na.rm = TRUE),
      .groups = "drop"
    )
    bad <- dplyr::filter(sides, n_home != 1 | n_away != 1)
    out$home_away <- flag(bad, "team_box_home_away", "Expected one home and one away row")
  }

  shot_cols <- c("team_score", "field_goals_made", "three_point_field_goals_made", "free_throws_made")
  if (has(team_box, shot_cols)) {
    implied <- 2 * team_box$field_goals_made + team_box$three_point_field_goals_made + team_box$free_throws_made
    bad <- team_box[!is.na(implied) & !is.na(team_box$team_score) & implied != team_box$team_score, ]
    out$points <- flag(bad, "team_points_mismatch", "team_score != 2*FGM + 3PM + FTM", bad$team_score)
  }

  made_attempted <- list(
    c("field_goals_made", "field_goals_attempted"),
    c("three_point_field_goals_made", "three_point_field_goals_attempted"),
    c("free_throws_made", "free_throws_attempted"),
    c("three_point_field_goals_made", "field_goals_made")
  )
  for (pair in made_attempted) {
    if (has(team_box, pair)) {
      bad <- team_box[(team_box[[pair[1]]] > team_box[[pair[2]]]) %in% TRUE, ]
      out[[paste(pair, collapse = "_gt_")]] <- flag(bad, "team_made_gt_attempted", paste(pair[1], ">", pair[2]))
    }
  }

  count_cols <- intersect(
    c("team_score", "assists", "steals", "blocks", "turnovers", "total_rebounds", "fouls"),
    names(team_box)
  )
  if (length(count_cols) > 0 && nrow(team_box) > 0) {
    negative <- Reduce(`|`, lapply(count_cols, function(col) (team_box[[col]] < 0) %in% TRUE))
    out$negative <- flag(team_box[negative, ], "team_negative_count", "Negative counting stat")
  }

  score_cols <- c("game_id", "home_team_score", "away_team_score")
  if (has(played_games, score_cols) && has(team_box, c("team_home_away", "team_score"))) {
    joined <- dplyr::inner_join(
      team_box[, c("game_id", "team_home_away", "team_score")],
      played_games[, score_cols],
      by = "game_id"
    )
    expected <- ifelse(joined$team_home_away == "home", joined$home_team_score, joined$away_team_score)
    bad <- joined[!is.na(expected) & expected != joined$team_score, ]
    out$game_score <- flag(bad, "team_score_vs_games", "team_box score differs from games score")
  }

  if (has(played_games, score_cols)) {
    bad <- played_games[(played_games$home_team_score == played_games$away_team_score) %in% TRUE, ]
    out$tie <- flag(bad, "games_tied_score", "Completed game with tied score")
  }

  if (has(player_box, c("game_id", "team_id", "points")) && has(team_box, c("team_id", "team_score"))) {
    player_pts <- dplyr::summarise(
      dplyr::group_by(player_box, game_id, team_id),
      player_points = sum(points, na.rm = TRUE),
      .groups = "drop"
    )
    joined <- dplyr::inner_join(
      player_pts,
      team_box[, c("game_id", "team_id", "team_score")],
      by = c("game_id", "team_id")
    )
    bad <- joined[(joined$player_points != joined$team_score) %in% TRUE, ]
    out$player_points <- flag(bad, "player_points_vs_team", "Sum of player points != team_score", bad$player_points)
  }

  dplyr::bind_rows(c(list(.espn_nba_empty_issues()), unname(out)))
}

.espn_nba_missingness_report <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tibble::tibble(
      column = names(df),
      n_missing = integer(length(names(df))),
      pct_missing = rep(NA_real_, length(names(df)))
    ))
  }

  n <- nrow(df)
  tibble::tibble(
    column = names(df),
    n_missing = vapply(df, function(x) sum(is.na(x)), integer(1)),
    pct_missing = vapply(df, function(x) sum(is.na(x)) / n, numeric(1))
  )
}

.espn_nba_sample_games <- function(games) {
  if (is.null(games) || nrow(games) == 0 || !"game_date" %in% names(games)) {
    return(list(early = integer(), mid = integer(), late = integer()))
  }

  dates <- sort(unique(stats::na.omit(games$game_date)))
  if (length(dates) == 0) {
    return(list(early = integer(), mid = integer(), late = integer()))
  }

  early_date <- dates[1]
  mid_date <- dates[ceiling(length(dates) / 2)]
  late_date <- dates[length(dates)]

  list(
    early = unique(games$game_id[games$game_date == early_date]),
    mid = unique(games$game_id[games$game_date == mid_date]),
    late = unique(games$game_id[games$game_date == late_date])
  )
}
