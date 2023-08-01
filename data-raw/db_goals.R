db_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/goals_df.csv",
  show_col_types = FALSE
)


# Send db_goals to /data directory

usethis::use_data(db_goals, overwrite = TRUE)

###########################################################
# A tibble: 7,043 × 5                                     #
# ------------------------------------------------------- #
# game_date | player_name | goal_min | penalty | own_goal #
# ------------------------------------------------------- #
#                                                         #
###########################################################
