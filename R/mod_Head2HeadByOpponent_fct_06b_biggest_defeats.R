get_biggest_defeats_by_opp <- function(df, biggest_diff=1) {

  df <- df %>%
    dplyr::filter(
      outcome == "L",
      goal_diff <= -biggest_diff
    ) %>%
    dplyr::arrange(
      goal_diff,
      dplyr::desc(goals_against),
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
