get_day_list <- function() {
  days <- c(1:31)
  return (days)
}

get_month_list <- function() {
  months <- lubridate::month(c(1:12), label = TRUE, abbr = FALSE)
  return (months)
}

get_year_list <- function() {
  df <- results_dataset %>%
  min_year <- min(lubridate::year(df$game_date))
  max_year <- max(lubridate::year(df$game_date))
  year_list <- c(min_year:max_year)
  return (year_list)
}

otd_results_df <- function(selected_day, selected_month) {
  df <- results_dataset %>%
    dplyr::mutate(
      day = lubridate::day(game_date),
      month = lubridate::month(game_date, label = FALSE)
    ) %>%
    dplyr::filter(
      day == selected_day,
      month == selected_month
    )
  return (df)
}
