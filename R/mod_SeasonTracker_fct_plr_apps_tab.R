selected_season <- "2023/24"

player_apps %>%
  dplyr::filter(
    season %in% selected_season
  ) %>%
  dplyr::left_join(
    results_dataset,
    by = c(
      "season",
      "game_date",
      "game_no"
    )
  ) %>%
  dplyr::mutate(
    generic_comp = dplyr::case_when(
      game_type == "League" ~ "League",
      game_type == "Cup" & !generic_comp %in% c("FA Cup", "League Cup") ~ "Other",
      TRUE ~ generic_comp
    )
  ) %>%
  dplyr::group_by(
    season,
    menu_name,
    generic_comp
  ) %>%
  dplyr::summarise(
    apps = dplyr::n(),
    starts = sum(role == "starter"),
    sub_apps = sum(role == "sub"),
    goals = sum(goals_scored),
    yellows = sum(yellow_cards),
    reds = sum(red_cards),
    mins_played = sum(mins_played),
    .groups = "drop"
  ) %>%
  # Long to wide
  tidyr::pivot_wider(
    id_cols = c(
      season,
      menu_name
    ),
    names_from = generic_comp,
    values_from = c(
      apps,
      starts,
      sub_apps,
      goals,
      yellows,
      reds,
      mins_played
    ),
    names_sep = "_"
  )
