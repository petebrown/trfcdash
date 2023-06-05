get_formatted_results <- function(selected_seasons) {
  results_df <-  filter_ssn_results(selected_seasons) %>%
    dplyr::select(
      season,
      ssn_game_no,
      game_date,
      opposition,
      venue,
      score,
      competition,
      manager,
      attendance
    ) %>%
    dplyr::rename(
      Season = season,
      "Game \nNo" = ssn_game_no,
      Date = game_date,
      Opponent = opposition,
      Venue = venue,
      Score = score,
      Division = competition,
      Manager = manager,
      Attendance = attendance
    )

  return (results_df)
}

output_ssn_results <- function(season) {
  DT::renderDataTable(
    get_formatted_results(season),
    selection = 'single',
    filter = 'bottom',
    rownames = FALSE,
    options = list(
      paging = TRUE,
      pageLength = 10,
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
