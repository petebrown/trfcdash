get_streaks_by_team <- function(opponent, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      opposition == opponent,
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
    )

  df %>%
    generate_streaks() %>%
    streaks_reactable()

}
