get_streaks <- function(selected_seasons, inc_cup_games, pens_as_draw) {
  streaks <- filter_ssn_results(selected_seasons)

  if (inc_cup_games == "No") {
    streaks <- streaks %>%
      dplyr::filter(game_type == "League")
  }

  streaks <- streaks %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::arrange(season, game_date) %>%
    dplyr::group_by(season) %>%
    generate_streaks() %>%
    dplyr::rename(Season = season) %>%

  return (streaks)
}

render_streaks <- function(selected_seasons, inc_cup_games, pens_as_draw) {
  df <- get_streaks(selected_seasons, inc_cup_games, pens_as_draw)

  streaks_reactable(df)
}
