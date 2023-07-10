output_h2h_streaks <- function(year_range, league_tiers, cup_comps, venue_options, min_games) {
  df <- results_dataset %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4))
    ) %>%
    dplyr::filter(
      ssn_year >= year_range[1],
      ssn_year <= year_range[2],
      venue %in% venue_options,
      league_tier %in% league_tiers | generic_comp %in% cup_comps
    ) %>%
    dplyr::arrange(
      season,
      ssn_game_no
    ) %>%
    dplyr::group_by(
      opposition
    ) %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths))) %>%
    dplyr::summarize(
      P = dplyr::n(),
      "Winning" = max(w_streak),
      "Unbeaten" = max(unbeaten_streak),
      "Losing" = max(losing_streak),
      "Winless" = max(winless_streak),
      "Drawing" = max(d_streak),
      "Clean sheets" = max(clean_sheets)
    ) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(Winning),
      dplyr::desc(P)
    ) %>%
    dplyr::rename(
      Opposition = opposition
    )
  return(df)
}
