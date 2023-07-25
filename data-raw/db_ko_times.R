db_ko_times <- results_dataset %>%
  dplyr::select(
    ko_time
  )

usethis::use_data(db_ko_times, overwrite = TRUE)
