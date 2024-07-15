get_biggest_defeats <- function(df, min_diff=4) {

  df <- df %>%
    dplyr::filter(
      outcome == "L",
      goal_diff <= -min_diff
    ) %>%
    dplyr::arrange(
      goal_diff,
      dplyr::desc(goals_against),
      game_date
    )
}
