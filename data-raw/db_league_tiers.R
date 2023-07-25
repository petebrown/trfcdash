db_league_tiers <- results_dataset %>%
  dplyr::filter(
    !is.na(league_tier)
  ) %>%
  dplyr::select(
    competition,
    league_tier
  ) %>%
  unique() %>%
  dplyr::arrange(
    competition
  )

usethis::use_data(db_league_tiers, overwrite = TRUE)
