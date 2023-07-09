get_season_list <- function() {
  df <- results_dataset
  return (unique(df$season))
}

get_chart_options <- function() {
  chart_options <- c("League Position" = "league_pos",
                     "Total points" = "pts",
                     "Points-per-game" = "ppg")
  return(chart_options)
}
