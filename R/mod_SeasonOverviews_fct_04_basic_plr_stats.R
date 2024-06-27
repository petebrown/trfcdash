get_basic_player_stats <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

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
      ssn_year = as.numeric(stringr::str_sub(season, end = 4)),
      game_year = lubridate::year(game_date),
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome,
      )
    ) %>%
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
    tidyr::replace_na(
      list(
        goals_scored = 0,
        mins_played = 0,
        yellow_cards = 0,
        red_cards = 0,
        mins_played = 0
      )
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      mins_played = sum(mins_played),
      mins_per_goal = mins_played / goals,
      games_per_goal = mins_per_goal / 90,
      yellow_cards = sum(yellow_cards),
      mins_per_yc = mins_played / yellow_cards,
      games_per_yc = mins_per_yc / 90,
      red_cards = sum(red_cards),
      mins_per_rc = mins_played / red_cards,
      games_per_rc = mins_per_rc / 90,
      mins_per_card = mins_played / (yellow_cards + red_cards),
      games_per_card = mins_per_card / 90
    ) %>%
    dplyr::mutate(
      total_apps = starts + sub_apps
    ) %>%
    dplyr::filter(
      starts >= min_games
    ) %>%
    dplyr::arrange(
      menu_name
    ) %>%
    dplyr::select(
      menu_name,
      total_apps,
      starts,
      sub_apps,
      mins_played,
      goals,
      mins_per_goal,
      games_per_goal,
      yellow_cards,
      mins_per_yc,
      games_per_yc,
      red_cards,
      mins_per_rc,
      games_per_rc,
      mins_per_card,
      games_per_card
    )

  reactable::reactable(
    data = df,
    columns = list(
      menu_name = reactable::colDef(
        name = "Player"
      ),
      total_apps = reactable::colDef(
        name = "Apps"
      ),
      starts = reactable::colDef(
        name = "Starts"
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps"
      ),
      mins_played = reactable::colDef(
        name = "Mins Played"
      ),
      goals = reactable::colDef(
        name = "Goals"
      ),
      mins_per_goal = reactable::colDef(
        name = "Mins/Goals"
      ),
      games_per_goal = reactable::colDef(
        name = "Games/Goal"
      ),
      yellow_cards = reactable::colDef(
        name = "YC"
      ),
      mins_per_yc = reactable::colDef(
        name = "Mins/YC"
      ),
      games_per_yc = reactable::colDef(
        name = "Games/YC"
      ),
      red_cards = reactable::colDef(
        name = "RC"
      ),
      mins_per_rc = reactable::colDef(
        name = "Mins/RC"
      ),
      games_per_rc = reactable::colDef(
        name = "Games/RC"
      ),
      mins_per_card = reactable::colDef(
        name = "Mins/Card"
      ),
      games_per_card = reactable::colDef(
        name = "Games/Card"
      )
    ),
    columnGroups = list(
      reactable::colGroup(
        name = "Appearances",
        columns = c(
          "total_apps",
          "starts",
          "sub_apps",
          "mins_played"
        )
      ),
      reactable::colGroup(
        name = "Goals",
        columns = c(
          "goals",
          "mins_per_goal",
          "games_per_goal"
        )
      ),
      reactable::colGroup(
        name = "Discipline",
        columns = c(
          "yellow_cards",
          "mins_per_yc",
          "games_per_yc",
          "red_cards",
          "mins_per_rc",
          "games_per_rc",
          "mins_per_card",
          "games_per_card"
        )
      )
    )
  )

}
