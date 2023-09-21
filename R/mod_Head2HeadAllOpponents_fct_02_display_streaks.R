output_h2h_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

  df <- results_dataset %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4))
    ) %>%
    dplyr::filter(
      ssn_year >= year_range[1],
      ssn_year <= year_range[2],
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
    dplyr::arrange(
      season,
      game_no
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
      "Wins" = max(w_streak),
      "Unbeaten" = max(unbeaten_streak),
      "Defeats" = max(losing_streak),
      "Winless" = max(winless_streak),
      "Draws" = max(d_streak),
      "Clean sheets" = max(clean_sheets)
    ) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(Wins),
      dplyr::desc(P)
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "Wins",
    columns = list(
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 130
      )
    )
  )
}
