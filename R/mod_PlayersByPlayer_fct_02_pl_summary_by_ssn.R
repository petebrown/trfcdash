output_pl_summary_by_ssn <- function(inp_player_name) {
  player_apps %>%
    dplyr::filter(
      menu_name == inp_player_name
    ) %>%
    tidyr::replace_na(
      list(
        goals_scored = 0,
        mins_played = 0,
        yellow_cards = 0,
        red_cards = 0,
        mins_played = 0
      )
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      yellow_cards = sum(yellow_cards),
      red_cards = sum(red_cards),
      mins_played = sum(mins_played)
    ) %>%
    dplyr::mutate(
      total_apps = starts + sub_apps,
      mins_played = format(mins_played, nsmall = 0, big.mark = ",")
    ) %>%
    dplyr::select(
      season,
      total_apps,
      starts,
      sub_apps,
      mins_played,
      goals,
      yellow_cards,
      red_cards
    ) %>%
    dplyr::rename(
      Season = season,
      Apps = total_apps,
      Starts = starts,
      "Sub Apps" = sub_apps,
      Goals = goals,
      "Mins played" = mins_played,
      "Yellow cards" = yellow_cards,
      "Red cards" = red_cards
    )
}
