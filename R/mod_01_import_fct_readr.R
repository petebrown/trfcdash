import_results_mini <- function() {
  df <- readr::read_csv("https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv")
  return (df)
}

import_results <- function() {
  df <- readr::read_csv('https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv')
  return (df)
}
