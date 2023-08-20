goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/goals.csv",
  show_col_types = FALSE
)

usethis::use_data(goals, overwrite = TRUE)

###########################################################
# A tibble: 7,043 × 5                                     #
# ------------------------------------------------------- #
# game_date | player_name | goal_min | penalty | own_goal #
# ------------------------------------------------------- #
#                                                         #
###########################################################

goalscorers_by_game <- goals %>%
  dplyr::select(
    game_date,
    player_name,
    penalty
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    pens = sum(penalty, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(game_date) %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      goals_scored == 1 & pens == 0 ~ player_name,
      goals_scored > 1 & pens == 0 ~ stringr::str_glue("{player_name} {goals_scored}"),
      goals_scored == 1 & pens == 1 ~ stringr::str_glue("{player_name} (pen)"),
      goals_scored > 1 & pens == 1 ~ stringr::str_glue("{player_name} {goals_scored} (1 pen)"),
      goals_scored > 1 & pens > 1 ~ stringr::str_glue("{player_name} {goals_scored} ({pens} pens)"),
      .default = stringr::str_glue("{player_name}"),
    ),
    scorers = paste(player_name, collapse = ", ")
  ) %>%
  dplyr::select(
    game_date,
    scorers
  ) %>%
  unique() %>%
  dplyr::arrange(
    game_date
  )

usethis::use_data(goalscorers_by_game, overwrite = TRUE)


# =======
# db_goals <- goals %>%
#   dplyr::filter(
#     season < 1996
#   ) %>%
#   dplyr::left_join(
#     game_dates_and_nos,
#     by = c(
#       "season",
#       "game_no" = "ssn_game_no"
#     )
#   ) %>%
#   dplyr::group_by(
#     season,
#     game_no,
#     player_name
#   ) %>%
#   dplyr::slice(
#     rep(1:dplyr::n(), each = goals_scored)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(
#     game_date,
#     player_name
#   ) %>%
#   dplyr::bind_rows(
#     sb_gf
#   ) %>%
#   dplyr::mutate(
#     own_goal = dplyr::case_match(
#       player_name,
#       "OG" ~ 1,
#       .default = own_goal
#     )
#   ) %>%
#   rbind(
#     fa_trophy_goals
#   ) %>%
#   dplyr::arrange(
#     game_date,
#     minute,
#     player_name
#   )
#
# usethis::use_data(db_goals, overwrite = TRUE)
