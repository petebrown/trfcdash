game_dates_table <- results_dataset %>%
  dplyr::select(
    game_date,
    season
  )

usethis::use_data(game_dates_table, overwrite = TRUE)
