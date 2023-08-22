results_dataset <- vroom::vroom(
    file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/results.csv",
    show_col_types = FALSE
  ) %>%
  dplyr::left_join(
    goalscorers_by_game,
    by = "game_date"
  ) %>%
  dplyr::mutate(
    ssn_year = as.numeric(stringr::str_sub(season, end = 4)),
    game_year = lubridate::year(game_date),
    game_month = lubridate::month(game_date),
    game_day = lubridate::day(game_date),
  )


game_lengths <- results_dataset %>%
  dplyr::select(
    game_date,
    game_length
  )


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
  results_dataset,
  game_lengths,
  season_game_dates,
  season_game_nos,

  overwrite = TRUE
)
