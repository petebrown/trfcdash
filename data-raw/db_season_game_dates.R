season_game_dates <- results_dataset %>%
  dplyr::select(
    game_date,
    season
  )

usethis::use_data(season_game_dates, overwrite = TRUE)
