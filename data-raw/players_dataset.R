player_apps_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/player_apps.csv",
  show_col_types = FALSE
)


player_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/player_apps.csv",
  show_col_types = FALSE
)


player_info <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/player_dobs.csv",
  show_col_types = FALSE
)


player_goals_per_game <- goals %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    .groups = "drop"
  )


player_apps <- player_apps %>%
  dplyr::left_join(
    season_game_nos,
    by = "game_date"
  ) %>%
  dplyr::left_join(
    season_game_dates,
    by = "game_date"
  ) %>%
  dplyr::left_join(
    player_goals_per_game,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::left_join(
    yellow_cards,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::left_join(
    red_cards,
    by = c(
      "game_date",
      "player_name"
    )
  )  %>%
  dplyr::left_join(
    subs,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::left_join(
    player_info,
    by = c(
      "player_name",
      "season"
    )
  ) %>%
  tidyr::replace_na(
    list(
      goals_scored = 0,
      yellow_cards = 0,
      red_cards = 0
    )
  ) %>%
  dplyr::left_join(
    game_lengths,
    by = "game_date"
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mins_played = dplyr::case_when(
      # Started, played to end
      role == "starter" & is.na(min_off) & is.na(min_so) ~ game_length,
      # Started, subbed off
      role == "starter" & !is.na(min_off) & is.na(min_so) ~ min_off,
      # Started, sent off
      role == "starter" & is.na(min_off) & !is.na(min_so) ~ min_so,
      # Subbed on, played to end
      role == "sub" & is.na(min_off) & is.na(min_so) ~ game_length - min_on,
      # Subbed on, subbed off
      role == "sub" & !is.na(min_off) & is.na(min_so) ~ min_off - min_on,
      # Subbed on, sent off
      role == "sub" & is.na(min_off) & !is.na(min_so) ~ min_so - min_on,
    )
  ) %>% dplyr::ungroup() %>%
  dplyr::rename(
    menu_name = pl_index
  ) %>%
  dplyr::select(
    season,
    game_no,
    game_date,
    player_name,
    role,
    goals_scored,
    mins_played,
    yellow_cards,
    red_cards,
    shirt_no,
    on_for,
    off_for,
    min_on,
    min_off,
    player_dob,
    dob_display,
    menu_name
  )


usethis::use_data(
  player_apps,
  player_info,

  overwrite = TRUE
)
