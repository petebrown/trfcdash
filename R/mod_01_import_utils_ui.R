get_season_list <- function() {
  df <- import_results()
  season_list <- unique(df$season)
  return(season_list)
}

get_chart_options <- function() {
  options <- c("League Position" = "league_pos",
               "Total points" = "pts",
               "Points-per-game" = "ppg")
  return(options)
}
