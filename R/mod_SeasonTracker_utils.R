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
