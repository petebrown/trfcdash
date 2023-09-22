mgr_durations <- function() {
  managers %>%
    dplyr::mutate(
      duration = difftime(date_to, date_from)
    )
}

total_mgr_durations <- function() {
  managers %>%
  dplyr::group_by(
    manager_name
  ) %>%
  dplyr::mutate(
    duration = difftime(date_to, date_from)
  ) %>%
  dplyr::summarise(
    duration = sum(duration)
  )
}
