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

  # reactable::reactable(
  #   data = df,
  #   class = "reactable-text",
  #   style = "font-size: smaller",
  #   columns = list(
  #     season = reactable::colDef(
  #       name = "Season"
  #     ),
  #     game_date = reactable::colDef(
  #       name = "Date",
  #       format = reactable::colFormat(
  #         date = TRUE,
  #         locales = "en-GB"
  #       )
  #     ),
  #     opposition = reactable::colDef(
  #       name = "Opponent"
  #     ),
  #     venue = reactable::colDef(
  #       name = "Venue"
  #     ),
  #     score = reactable::colDef(
  #       name = "Score"
  #     ),
  #     outcome = reactable::colDef(
  #       name = "Res"
  #     ),
  #     competition = reactable::colDef(
  #       name = "Competition"
  #     ),
  #     attendance = reactable::colDef(
  #       name = "Att",
  #       format = reactable::colFormat(
  #         digits = 0,
  #         separators = TRUE
  #       )
  #     ),
  #     manager = reactable::colDef(
  #       name = "Manager"
  #     )
  #   )
  # )
}
