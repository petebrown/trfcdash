get_season_list <- function() {
  df <- results_dataset %>%
    dplyr::arrange(
      dplyr::desc(game_date)
    )
  return (unique(df$season))
}

get_chart_options <- function() {
  chart_options <- c("League Position" = "league_pos",
                     "Total points" = "pts",
                     "Points-per-game" = "ppg")
  return(chart_options)
}

get_manager_list <- function() {
  df <- results_dataset %>%
    dplyr::arrange(
      dplyr::desc(game_date)
    )
  return (unique(df$manager))
}

get_div_list_by_tier <- function(selected_tiers) {
  ssn_list <- results_dataset %>%
    dplyr::arrange(
      dplyr::desc(game_date)
    ) %>%
    dplyr::filter(
      league_tier %in% selected_tiers,
      game_type == "League"
    ) %>%
    dplyr::select(
      season
    ) %>%
    dplyr::distinct()

  return (ssn_list$season)
}

get_comps_by_season <- function(selected_seasons) {
  comp_list <- results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::select(
      generic_comp
    ) %>%
    dplyr::distinct()

  return (comp_list$generic_comp)
}

base_season_tracker_df <- function(selected_seasons) {
  df <- results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons
    )
  return (df)
}
