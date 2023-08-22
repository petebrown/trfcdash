goals_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/goals.csv",
  show_col_types = FALSE
)


goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/goals.csv",
  show_col_types = FALSE
)


player_goals_per_game <- goals %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      .default = player_name,
      own_goal > 0 & player_name != "OG" ~ stringr::str_glue("{player_name} (OG)")
    )
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    pens = sum(penalty, na.rm = TRUE),
    .groups = "drop"
  )


goalscorers_by_game <- goals %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    pens = sum(penalty, na.rm = TRUE),
    own_goals = sum(own_goal, na.rm = TRUE),
    min_goal_min = min(goal_min),
    .groups = "drop"
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      goals_scored == 1 & pens == 0 & own_goals == 0 ~ player_name,
      goals_scored > 1 & pens == 0 & own_goals == 0 ~ stringr::str_glue("{player_name} {goals_scored}"),
      goals_scored == 1 & pens == 1 & own_goals == 0 ~ stringr::str_glue("{player_name} (pen)"),
      goals_scored > 1 & pens == 1 & own_goals == 0 ~ stringr::str_glue("{player_name} {goals_scored} (1 pen)"),
      goals_scored > 1 & pens > 1 & own_goals == 0 ~ stringr::str_glue("{player_name} {goals_scored} ({pens} pens)"),
      own_goals > 0 & player_name != "OG" ~ stringr::str_glue("{player_name} (OG)"),
      .default = player_name
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    game_date,
    min_goal_min
  ) %>%
  dplyr::group_by(
    game_date
  ) %>%
  dplyr::summarise(
    scorers = paste(player_name, collapse = ", ")
  ) %>%
  dplyr::select(
    game_date,
    scorers
  )


usethis::use_data(
  goals,
  player_goals_per_game,
  goalscorers_by_game,

  overwrite = TRUE
)
