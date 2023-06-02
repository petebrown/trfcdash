import_results_mini <- function() {
  df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv", show_col_types = FALSE)
  return (df)
}

import_results <- function() {
  df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv", show_col_types = FALSE)
  return (df)
}

import_goals_df <- function() {
  df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/scrape-goals/main/data/goals.csv", show_col_types = FALSE)
  return (df)
}

import_players_df <- function() {
  df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv", show_col_types = FALSE) %>%
    dplyr::rename(
      game_id = sb_game_id,
      player_id = sb_player_id
    ) %>%
    dplyr::mutate(
      game_id = stringr::str_replace(game_id, "tpg", ""),
      game_id = as.numeric(game_id)
    )
  return (df)
}

imported_results_mini <- import_results_mini()
imported_results <- import_results()
imported_goals_df <- import_goals_df()
imported_players_df <- import_players_df()

get_season_list <- function() {
  df <- imported_results
  season_list <- unique(df$season)
  return (season_list)
}

get_chart_options <- function() {
  options <- c("League Position" = "league_pos",
               "Total points" = "pts",
               "Points-per-game" = "ppg")
  return (options)
}
