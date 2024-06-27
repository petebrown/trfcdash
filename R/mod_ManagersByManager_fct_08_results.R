get_manager_results <- function(manager_name) {
  results_dataset %>%
    dplyr::filter(
      manager == manager_name
    ) %>%
    dplyr::arrange(
      game_date
    ) %>%
    dplyr::mutate(
      game_no = dplyr::row_number(),
      venue = factor(venue, levels = c("H", "A", "N"), ordered=TRUE)
    ) %>%
    dplyr::select(
      game_no,
      season,
      game_date,
      venue,
      opposition,
      outcome,
      score,
      generic_comp,
      league_pos,
      attendance
    )
}

output_mgr_games <- function(manager_name) {
  df <- get_manager_results(manager_name)

  compile_results_table(df, 'manager')

}
