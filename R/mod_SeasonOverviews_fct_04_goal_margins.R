get_goal_margins <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {

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
    dplyr::ungroup() %>%
    dplyr::mutate(
      goal_margin = goals_for - goals_against,
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      plus_1 = sum(goal_margin == 1),
      plus_2 = sum(goal_margin == 2),
      plus_3 = sum(goal_margin == 3),
      plus_4 = sum(goal_margin == 4),
      over_4 = sum(goal_margin > 4),
      zero = sum(goal_margin == 0),
      minus_1 = sum(goal_margin == -1),
      minus_2 = sum(goal_margin == -2),
      minus_3 = sum(goal_margin == -3),
      minus_4 = sum(goal_margin == -4),
      under_4 = sum(goal_margin < -4)
    )

  reactable::reactable(
    data = df,
    columns = list(
      season = reactable::colDef(
        name = "Season"
      ),
      plus_1 = reactable::colDef(
        name = "+1"
      ),
      plus_2 = reactable::colDef(
        name = "+2"
      ),
      plus_3 = reactable::colDef(
        name = "+3"
      ),
      plus_4 = reactable::colDef(
        name = "+4"
      ),
      over_4 = reactable::colDef(
        name = ">=5"
      ),
      zero = reactable::colDef(
        name = "0"
      ),
      minus_1 = reactable::colDef(
        name = "-1"
      ),
      minus_2 = reactable::colDef(
        name = "-2"
      ),
      minus_3 = reactable::colDef(
        name = "-3"
      ),
      minus_4 = reactable::colDef(
        name = "-4"
      ),
      under_4 = reactable::colDef(
        name = "<=-5"
      )
    )
  )
}
