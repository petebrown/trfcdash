output_h2h_records <- function(year_range, league_tiers, cup_comps, venue_options, min_games) {
  df <- results_dataset %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4))
    ) %>%
      dplyr::filter(
        ssn_year >= year_range[1],
        ssn_year <= year_range[2]
      ) %>%
      dplyr::filter(
        league_tier %in% league_tiers | generic_comp %in% cup_comps,
        venue %in% venue_options
      ) %>%
      dplyr::group_by(
        opposition
      ) %>%
      dplyr::summarize(
        P = dplyr::n(),
        W = sum(outcome == "W"),
        D = sum(outcome == "D"),
        L = sum(outcome == "L"),
        GF = sum(goals_for),
        GA = sum(goals_against)
      ) %>%
      dplyr::mutate(
        win_pc = round((W / P) * 100, 2)
      ) %>%
      dplyr::filter(
        P >= min_games
      ) %>%
      dplyr::arrange(
        dplyr::desc(win_pc),
        P,
        opposition
      ) %>%
      dplyr::rename(
        Opposition = opposition,
        "Win %" = win_pc
      )
  return(df)
}
