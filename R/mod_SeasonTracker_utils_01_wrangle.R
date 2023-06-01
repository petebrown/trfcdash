import_results_mini <- function() {
  df <- readr::read_csv("https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv")
  return (df)
}


import_results <- function() {
  df <- readr::read_csv('https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv')
  return (df)
}


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
