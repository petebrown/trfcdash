get_biggest_wins_by_opp <- function(df, biggest_diff=1) {

  df <- df %>%
    dplyr::filter(
      outcome == "W",
      goal_diff >= biggest_diff
    ) %>%
    dplyr::arrange(
      dplyr::desc(goal_diff),
      dplyr::desc(goals_for),
      game_date
    ) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      venue,
      opposition,
      outcome,
      score,
      game_type,
      competition,
      attendance,
      manager,
      league_tier
    )
}
