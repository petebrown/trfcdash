get_basic_ssn_stats <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  goals <- results_dataset %>%
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
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarise(
      games = dplyr::n(),
      gf = sum(goals_for),
      goals_per_game = gf / games,
      ga = sum(goals_against),
      ga_per_game = ga / games,
      clean_sheets = sum(goals_against == 0),
      clean_sheet_pct = clean_sheets / games,
      games_per_cs = games / clean_sheets
    )

  cards <- player_apps %>%
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
      season
    ) %>%
    dplyr::summarise(
      yc = sum(yellow_cards),
      rc = sum(red_cards),
      cards = yc + rc
    )

  df <- goals %>%
    dplyr::left_join(
      cards,
      by = c(
        "season"
      )
    )

  reactable::reactable(df)

  # reactable::reactable(
  #   data = df,
  #   columns = list(
  #     menu_name = reactable::colDef(
  #       name = "Player"
  #     ),
  #     total_apps = reactable::colDef(
  #       name = "Apps"
  #     ),
  #     starts = reactable::colDef(
  #       name = "Starts"
  #     ),
  #     sub_apps = reactable::colDef(
  #       name = "Sub Apps"
  #     ),
  #     mins_played = reactable::colDef(
  #       name = "Mins Played"
  #     ),
  #     goals = reactable::colDef(
  #       name = "Goals"
  #     ),
  #     mins_per_goal = reactable::colDef(
  #       name = "Mins/Goals"
  #     ),
  #     games_per_goal = reactable::colDef(
  #       name = "Games/Goal"
  #     ),
  #     yellow_cards = reactable::colDef(
  #       name = "YC"
  #     ),
  #     mins_per_yc = reactable::colDef(
  #       name = "Mins/YC"
  #     ),
  #     games_per_yc = reactable::colDef(
  #       name = "Games/YC"
  #     ),
  #     red_cards = reactable::colDef(
  #       name = "RC"
  #     ),
  #     mins_per_rc = reactable::colDef(
  #       name = "Mins/RC"
  #     ),
  #     games_per_rc = reactable::colDef(
  #       name = "Games/RC"
  #     ),
  #     mins_per_card = reactable::colDef(
  #       name = "Mins/Card"
  #     ),
  #     games_per_card = reactable::colDef(
  #       name = "Games/Card"
  #     )
  #   ),
  #   columnGroups = list(
  #     reactable::colGroup(
  #       name = "Appearances",
  #       columns = c(
  #         "total_apps",
  #         "starts",
  #         "sub_apps",
  #         "mins_played"
  #       )
  #     ),
  #     reactable::colGroup(
  #       name = "Goals",
  #       columns = c(
  #         "goals",
  #         "mins_per_goal",
  #         "games_per_goal"
  #       )
  #     ),
  #     reactable::colGroup(
  #       name = "Discipline",
  #       columns = c(
  #         "yellow_cards",
  #         "mins_per_yc",
  #         "games_per_yc",
  #         "red_cards",
  #         "mins_per_rc",
  #         "games_per_rc",
  #         "mins_per_card",
  #         "games_per_card"
  #       )
  #     )
  #   )
  # )

}

# get_basic_ssn_stats(year_range = c(2023,2023), league_tiers = c(2,3,4,5), includePlayOffs = ("Yes"), cup_comps = c("Anglo-Italian Cup", "Associate Members\' Cup", "FA Cup", "FA Trophy", "Full Members\' Cup", "League Cup", "War League"), pens_as_draw = "Yes", venue_options = c("H", "A", "N"), min_games = 1)
