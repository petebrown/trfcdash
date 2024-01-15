get_otd_results <- function(otd_date, inc_year = "No", as_reactable = "Yes") {

  df <- results_dataset %>%
    dplyr::filter(
      lubridate::month(game_date) == lubridate::month(otd_date),
      lubridate::day(game_date) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(game_date) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    )

  if (as_reactable == "Yes") {
    reactable::reactable(df)
  } else {
    return(df)
  }
}
