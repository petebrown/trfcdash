#' utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
import_results_mini <- vroom::vroom(
    file = "https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv",
    col_select = c("game_date", "ranking", "pts"),
    show_col_types = FALSE)

import_results <- vroom::vroom(
    file = "https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv",
    col_select = c("game_date", "season", "opposition", "venue", "home_team", "away_team", "score", "secondary_score", "outcome", "competition", "goals_for", "goals_against", "attendance", "league_tier", "generic_comp", "game_type", "ssn_game_no", "ssn_comp_game_no", "weekday", "manager"),
    show_col_types = FALSE)

import_goals <- vroom::vroom(
    file = "https://raw.githubusercontent.com/petebrown/scrape-goals/main/data/goals.csv",
    col_select = c("game_id", "player_id", "player_name", "minute", "penalty", "own_goal", "goal_type"),
    show_col_types = FALSE
  )

import_player_apps <- vroom::vroom(
    file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
    show_col_types = FALSE
  )

import_managers <- vroom::vroom(
    file = "https://raw.githubusercontent.com/petebrown/scrape-managers/main/data/managers_df.csv",
    show_col_types = FALSE
  )

# Clean, join, wrangle imported datasets
get_results_df <- function() {
  df <- dplyr::full_join(
    x = import_results,
    y = import_results_mini,
    by = "game_date"
  ) %>%
  dplyr::rename(
    league_pos = ranking
  )
  return (df)
}

get_player_apps_df <- function() {
  df <- import_player_apps
  return (df)
}

get_goals_df <- function() {
  df <- import_goals
  return (df)
}

get_managers_df <- function() {
  df <- import_managers %>%
    dplyr::rename(manager_code = manager_sb_code)
  return (df)
}

# Basic data manipulation functions used across dashboard
filter_ssn_results <- function(selected_seasons) {
  results_df <- get_results_df() %>%
    dplyr::filter(season %in% selected_seasons)

  return (results_df)
}
