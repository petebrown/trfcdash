

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
    ) %>%
  dplyr::left_join(
    goalscorers_by_game,
    by = "game_date"
  )

usethis::use_data(db_results, overwrite = TRUE)
