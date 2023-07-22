fa_trophy_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/fa-trophy/fa_trophy_goals.csv",
  show_col_types = FALSE
)

sb_gf <- sb_goals %>%
  fix_sb_player_names() %>%
  dplyr::left_join(
    game_ids_and_dates,
    by = "game_id"
  ) %>%
  dplyr::filter(
    goal_type == "for"
  ) %>%
  dplyr::select(
    game_date,
    player_name,
    minute,
    penalty,
    own_goal
  )

goals_table <- goals %>%
  dplyr::filter(
    season < 1996
  ) %>%
  dplyr::left_join(
    game_dates_and_nos,
    by = c(
      "season",
      "game_no" = "ssn_game_no"
    )
  ) %>%
  dplyr::group_by(
    season,
    game_no,
    player_name
  ) %>%
  dplyr::slice(
    rep(1:dplyr::n(), each = goals_scored)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    game_date,
    player_name
  ) %>%
  dplyr::bind_rows(
    sb_gf
  ) %>%
  dplyr::mutate(
    own_goal = dplyr::case_match(
      player_name,
      "OG" ~ 1,
      .default = own_goal
    )
  ) %>%
  rbind(
    fa_trophy_goals
  ) %>%
  dplyr::arrange(
    game_date,
    minute,
    player_name
  )

usethis::use_data(goals_table, overwrite = TRUE)
