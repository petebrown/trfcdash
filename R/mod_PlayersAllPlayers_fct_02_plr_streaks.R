output_all_plr_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- player_apps %>%
    dplyr::left_join(
      results_dataset,
      by = c(
        "season",
        "game_date",
        "game_no"
      )
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome,
      )
    ) %>%
    dplyr::filter(
      role == "starter",
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::arrange(
      game_date
    ) %>%
    dplyr::mutate(
      goals = ifelse(goals_scored > 0, 1, 0),
      yellow_cards = ifelse(yellow_cards > 0, 1, 0),
      red_cards = ifelse(red_cards > 0, 1, 0),
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      goal_streak = ifelse(goals == 0, 0, sequence(rle(as.character(goals))$lengths)),
      yc_streak = ifelse(yellow_cards == 0, 0, sequence(rle(as.character(yellow_cards))$lengths)),
      rc_streak = ifelse(red_cards == 0, 0, sequence(rle(as.character(red_cards))$lengths)),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
      wins_to_0 = ifelse(wins_cs == 0, 0, sequence(rle(as.character(wins_cs))$lengths))
    ) %>%
    dplyr::summarize(
      starts = dplyr::n(),
      goals = max(goal_streak),
      yel_cards = max(yc_streak),
      red_cards = max(rc_streak),
      wins = max(w_streak),
      unbeaten = max(unbeaten_streak),
      losses = max(losing_streak),
      winless = max(winless_streak),
      draws = max(d_streak),
      clean_sheets = max(clean_sheets),
      wins_to_0 = max(wins_to_0),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      starts >= min_games,
    ) %>%
    dplyr::arrange(
      dplyr::desc(starts)
    ) %>%
    dplyr::select(
      menu_name,
      starts,
      wins,
      goals,
      yel_cards,
      red_cards,
      unbeaten,
      losses,
      winless,
      draws,
      clean_sheets,
      wins_to_0
    )

  output_tab <- reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    defaultSorted = list("wins" = "desc"),
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        minWidth = 150,
        cell = function(value) {
          plr_name_and_headshot(value)
        }
      ),
      starts = reactable::colDef(
        name = "Total Starts"
      ),
      wins = reactable::colDef(
        name = "Wins"
      ),
      goals = reactable::colDef(
        name = "Goals"
      ),
      yel_cards = reactable::colDef(
        name = "Yellow Cards"
      ),
      red_cards = reactable::colDef(
        name = "Red Cards"
      ),
      unbeaten = reactable::colDef(
        name = "Unbeaten"
      ),
      losses = reactable::colDef(
        name = "Losses"
      ),
      winless = reactable::colDef(
        name = "Winless"
      ),
      draws = reactable::colDef(
        name = "Draws"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets"
      ),
      wins_to_0 = reactable::colDef(
        name = "Wins to nil"
      )
    )
  )

  return(output_tab)
}
