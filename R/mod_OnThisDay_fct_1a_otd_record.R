get_otd_record <- function(otd_date, otd_inc_year) {

  df <- get_otd_results(otd_date, otd_inc_year, as_reactable="No") %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA,
      win_pc = (W / P),
      .groups = "drop"
    )

  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(
      align = "center"
    ),
    columns = list(
      win_pc = reactable::colDef(
        name = "Win %",
        align = "right",
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      )
    )
  )
}
