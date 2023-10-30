get_streaks <- function(selected_seasons) {
  streaks <- filter_ssn_results(selected_seasons) %>%
    dplyr::arrange(season, game_date) %>%
    dplyr::group_by(season) %>%
    generate_streaks() %>%
    dplyr::rename(Season = season) %>%

  return (streaks)
}

render_streaks <- function(selected_seasons) {
  df <- get_streaks(selected_seasons)

  streaks_reactable(df)
}
