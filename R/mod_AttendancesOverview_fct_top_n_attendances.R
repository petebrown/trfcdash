top_n_attendances <- function(year_range, n, comps, venues) {
  df <- results_dataset %>%
    dplyr::filter(
      !is.na(attendance),
      ssn_year >= year_range[1],
      ssn_year <= year_range[2],
      generic_comp %in% comps,
      venue %in% venues
    ) %>%
    dplyr::slice_max(
      order_by = attendance,
      by = season,
      n = n
    ) %>%
    dplyr::arrange(
      desc(attendance)
    )

  reactable::reactable(
    data = df
  )
}
