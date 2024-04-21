output_player_records <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

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
      debut = min(game_date),
      yellow_cards = sum(yellow_cards),
      red_cards = sum(red_cards),
      mins_played = sum(mins_played)
    ) %>%
    dplyr::mutate(
      # surname = stringr::str_split_i(player_name, " ", -1),
      # forename = stringr::str_remove(player_name, surname),
      # forename = stringr::str_trim(forename),
      total_apps = starts + sub_apps,
      debut = as.Date(debut, format = '%d-%m-%Y'),
      debut = format(debut, "%d/%m/%Y")
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
      goals,
      mins_played,
      yellow_cards,
      red_cards,
      debut
    )

  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    defaultSorted = list("total_apps" = "desc"),
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        show = TRUE,
        minWidth = 150,
        cell = function(value) {
            plr_name_and_img(value)
          }
      ),
      total_apps = reactable::colDef(
        name = "Apps",
        show = TRUE
      ),
      starts = reactable::colDef(
        name = "Starts",
        show = TRUE
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
        show = TRUE
      ),
      goals = reactable::colDef(
        name = "Goals",
        show = TRUE
      ),
      mins_played = reactable::colDef(
        name = "Mins Played",
        show = TRUE,
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      yellow_cards = reactable::colDef(
        name = "Yellow Cards",
        show = TRUE
      ),
      red_cards = reactable::colDef(
        name = "Red Cards",
        show = TRUE
      ),
      debut = reactable::colDef(
        name = "Debut",
        show = TRUE,
        align = "right"
      )
    )
  )
}
