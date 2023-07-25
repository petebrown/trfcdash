db_season_game_dates <- results_dataset %>%
  dplyr::select(
    game_date,
    season
  )

usethis::use_data(db_season_game_dates, overwrite = TRUE)
