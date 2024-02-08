get_attack_and_defend <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {

  scorers_df <- player_apps %>%
    dplyr::filter(
      goals_scored > 0
    ) %>%
    dplyr::select(
      game_date,
      menu_name,
      goals_scored
    )

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
    dplyr::left_join(
      scorers_df,
      by = c("game_date")
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarise(
      played = dplyr::n(),
      scored = sum(goals_for > 0),
      av_gf = mean(goals_for),
      diff_scorers = dplyr::n_distinct(menu_name),
      blanks = sum(goals_for == 0),
      clean_sheets = sum(goals_against == 0),
      av_ga = mean(goals_against)
    )

  reactable::reactable(
    df,
    columns = list(
      season = reactable::colDef(
        name = "Season"
      ),
      played = reactable::colDef(
        show = FALSE
      ),
      scored = reactable::colDef(
        name = "Goals Scored",
        align = "center"
      ),
      av_gf = reactable::colDef(
        name = "Av. Goals For",
        align = "center",
        format = reactable::colFormat(
          digits = 1
        )
      ),
      diff_scorers = reactable::colDef(
        name = "Different Scorers",
        align = "center"
      ),
      blanks = reactable::colDef(
        name = "Blanks",
        align = "center"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets",
        align = "center"
      ),
      av_ga = reactable::colDef(
        name = "Av. Goals Against",
        align = "center",
        format = reactable::colFormat(
          digits = 1
        )
      )
    )
  )
}
