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
      mins_played,
      goals,
      yellow_cards,
      red_cards,
      debut
    )

  player_headshots <- player_imgs %>%
    dplyr::select(menu_name=pl_index, plr_headshot=headshot_file_path)
  plr_positions <- player_positions %>%
    dplyr::select(menu_name=pl_index, position)

  df <- df %>%
    dplyr::left_join(
      player_headshots,
      by = "menu_name"
    ) %>%
    dplyr::left_join(
      plr_positions,
      by = "menu_name"
    )

  reactable::reactable(
    data = df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = list("total_apps" = "desc"),
    defaultColDef = reactable::colDef(
      vAlign = "center",
      minWidth = 80,
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    compact = TRUE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(nrow(df)),
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        minWidth = 180,
        cell = plr_name_and_headshot(),
        html = TRUE
      ),
      total_apps = reactable::colDef(
        name = "Apps",
      ),
      starts = reactable::colDef(
        name = "Starts",
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
      ),
      goals = reactable::colDef(
        name = "Goals",
      ),
      mins_played = reactable::colDef(
        name = "Mins Played",
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      yellow_cards = reactable::colDef(
        name = "🟨",
        minWidth = 60
      ),
      red_cards = reactable::colDef(
        name = "🟥",
        minWidth = 60
      ),
      debut = reactable::colDef(
        name = "Debut",
        align = "right",
        minWidth = 90
      ),
      plr_headshot = reactable::colDef(
        show = FALSE
      ),
      position = reactable::colDef(
        show = FALSE
      )
    )
  )
}
