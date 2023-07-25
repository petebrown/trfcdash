db_results <- results_dataset %>%
  dplyr::select(
    game_date,
    opposition,
    venue,
    score,
    outcome,
    goals_for,
    goals_against,
    outcome,
    competition,
    attendance
    )

usethis::use_data(db_results, overwrite = TRUE)
