#' @keywords internal
#' @importFrom magrittr %>%
"_PACKAGE"

# Column names used in dplyr verbs.
utils::globalVariables(c(
  "athlete_id", "didNotPlay", "dup_count", "file_path", "game_date",
  "game_id", "home_away", "issue", "n_away", "n_home", "player_rows", "points",
  "scraped", "severity", "team_home_away",
  "status_completed", "status_name", "status_state", "team_id", "team_rows"
))
