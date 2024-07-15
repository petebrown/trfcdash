get_biggest_wins <- function(df, biggest_diff=4) {

  df <- df %>%
    dplyr::filter(
      outcome == "W",
      goal_diff >= biggest_diff
    ) %>%
    dplyr::arrange(
      dplyr::desc(goal_diff),
      dplyr::desc(goals_for),
      game_date
    )

  return(df)
}
