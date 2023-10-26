get_season_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  min_game_no <- game_range[1]
  max_game_no <- game_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::arrange(game_date) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    ) %>%
    dplyr::filter(
      game_no >= min_game_no,
      game_no <= max_game_no
    ) %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      defeats_to_0 = ifelse(outcome == "L" & goals_for == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
      wins_to_nil = ifelse(wins_cs == 0, 0, sequence(rle(as.character(wins_cs))$lengths)),
      defeats_to_nil = ifelse(defeats_to_0 == 0, 0, sequence(rle(as.character(defeats_to_0))$lengths))
    ) %>%
    dplyr::rename(Season = season) %>%
    dplyr::summarize(
      wins = max(w_streak),
      unbeaten = max(unbeaten_streak),
      clean_sheets = max(clean_sheets),
      wins_to_nil = max(wins_to_nil),
      draws = max(d_streak),
      defeats = max(losing_streak),
      winless = max(winless_streak),
      defeats_to_nil = max(defeats_to_nil)
    )

  streaks_reactable(df)

}
