db_generic_comps <- results_dataset %>%
  dplyr::select(
    competition,
    generic_comp
  ) %>%
  unique() %>%
  dplyr::arrange(
    competition
  )

usethis::use_data(db_generic_comps, overwrite = TRUE)
