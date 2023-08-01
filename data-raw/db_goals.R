db_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/goals_df.csv",
  show_col_types = FALSE
)


<<<<<<< HEAD
# Send db_goals to /data directory

usethis::use_data(db_goals, overwrite = TRUE)

###########################################################
# A tibble: 7,043 × 5                                     #
# ------------------------------------------------------- #
# game_date | player_name | goal_min | penalty | own_goal #
# ------------------------------------------------------- #
#                                                         #
###########################################################
=======
db_goals <- goals %>%
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

usethis::use_data(db_goals, overwrite = TRUE)
>>>>>>> refs/remotes/origin/main
