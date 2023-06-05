get_otd_results <- function(selected_day, selected_month) {
  print(selected_day)
  selected_month <- as.character(match(selected_month, month.name))
  print(selected_month)

  df <- otd_results_df(selected_day, selected_month)
  return (df)
}
