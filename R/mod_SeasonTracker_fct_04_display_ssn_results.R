get_formatted_results <- function(selected_seasons) {
  results_df <-  filter_ssn_results(selected_seasons) %>%
    dplyr::select(
      season,
      ssn_game_no,
      game_date,
      opposition,
      venue,
      outcome,
      score,
      competition,
      attendance,
      manager
    ) %>%
    dplyr::mutate(
      game_date = glue::glue("{lubridate::day(game_date)} {lubridate::month(game_date, label = TRUE, abbr = FALSE)} {lubridate::year(game_date)}"),
      attendance = format(attendance, nsmall = 0, big.mark = ",")
    ) %>%
    dplyr::rename(
      Season = season,
      "Game \nNo" = ssn_game_no,
      Date = game_date,
      Opponent = opposition,
      Venue = venue,
      Outcome = outcome,
      Score = score,
      Competition = competition,
      Attendance = attendance,
      Manager = manager
    )

  return (results_df)
}

output_ssn_results <- function(season, n_fixtures) {
  DT::renderDataTable(
    get_formatted_results(season),
    selection = 'single',
    filter = 'bottom',
    rownames = FALSE,
    options = list(
      fillContainer = TRUE,
      paging = TRUE,
      pageLength = n_fixtures,
      info = TRUE,
      scrollX = TRUE,
      dom = 'frtip',
      columnDefs = list(
        list(targets = c(0, 1, 2, 3, 4, 6, 7), className = 'dt-left'),
        list(targets = c(5), className = 'dt-center'),
        list(targets = c(8), className = 'dt-right'))
    )
  )
}
