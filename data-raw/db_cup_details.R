db_cup_details <- results_dataset %>%
  dplyr::filter(
    game_type != "League"
  ) %>%
  dplyr::select(
    game_date,
    cup_round: gg_outcome
  )

usethis::use_data(db_cup_details, overwrite = TRUE)
