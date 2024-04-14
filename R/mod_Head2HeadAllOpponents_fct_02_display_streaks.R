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
    generate_streaks(drop_games_played = FALSE) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(wins),
      dplyr::desc(P)
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "wins",
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    columns = c(
      list(
        opposition = reactable::colDef(
          name = "Opposition",
          minWidth = 175,
          cell = function(value) {
            club_and_crest(value)
          }
        ),
        P = reactable::colDef(
          show = FALSE
        )
      ),
      format_streak_cols()
    )
  )
}
