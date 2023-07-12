output_player_summary <- function(inp_player_name) {
  player_apps %>%
    dplyr::filter(
      player_name == inp_player_name
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
    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      debut = min(game_date),
      yellow_cards = sum(yellow_cards),
      red_cards = sum(red_cards),
      mins_played = sum(mins_played)
    ) %>%
    dplyr::mutate(
      total_apps = starts + sub_apps,
      debut = as.Date(debut, format = '%d-%m-%Y'),
      debut = format(debut, "%d/%m/%Y"),
      mins_played = format(mins_played, nsmall = 0, big.mark = ",")
    ) %>%
    dplyr::select(
      total_apps,
      starts,
      sub_apps,
      goals,
      yellow_cards,
      red_cards,
      mins_played,
      debut
    ) %>%
    dplyr::rename(
      Apps = total_apps,
      Starts = starts,
      "Sub Apps" = sub_apps,
      Goals = goals,
      Debut = debut,
      "Mins played" = mins_played,
      "Yellow cards" = yellow_cards,
      "Red cards" = red_cards
    )
}
