top_n_attendances <- function(year_range, cup_comps, n=1, venues="H") {

  df <- results_dataset %>%
    dplyr::filter(
      !is.na(attendance),
      ssn_year >= year_range[1],
      ssn_year <= year_range[2],
      generic_comp %in% c("Football League", "Non-League", cup_comps),
      venue %in% venues
    ) %>%
    dplyr::slice_max(
      order_by = attendance,
      by = season,
      n = n
    ) %>%
    dplyr::arrange(
      desc(attendance)
    ) %>%
    dplyr::select(
      season,
      game_date,
      opposition,
      venue,
      score,
      outcome,
      generic_comp,
      Manager = manager,
      attendance
    )

  compile_results_table(df, page='attendance')
}
