season_game_dates <- results_dataset %>%
  dplyr::select(
    game_date,
    season
  )

season_game_nos <- results_dataset %>%
  dplyr::select(
    game_date,
    game_no
  )

usethis::use_data(
  season_game_dates,
  season_game_nos,
  overwrite = TRUE
)
