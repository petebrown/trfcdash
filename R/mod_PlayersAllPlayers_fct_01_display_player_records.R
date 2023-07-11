output_player_records <- function(year_range) {
  player_apps %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4)),
      game_year = lubridate::year(game_date)
    ) %>%
    dplyr::filter(
      ssn_year >= year_range[1],
      ssn_year <= year_range[2]
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
      player_name
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      debut = min(game_date),
      yellow_cards = sum(yellow_cards),
      red_cards = sum(red_cards),
      total_mins_played = sum(mins_played)
    ) %>%
    dplyr::mutate(
      surname = stringr::str_split_i(player_name, " ", -1),
      forename = stringr::str_remove(player_name, surname),
      forename = stringr::str_trim(forename),
      total_apps = starts + sub_apps
    ) %>%
    dplyr::arrange(
      surname,
      forename
    ) %>%
    dplyr::select(
      surname,
      forename,
      total_apps,
      starts,
      sub_apps,
      goals,
      debut,
      total_mins_played,
      yellow_cards,
      red_cards
    ) %>%
    dplyr::rename(
      Surname = surname,
      Forename = forename,
      "Total Apps" = total_apps,
      Starts = starts,
      "Sub Apps" = sub_apps,
      Goals = goals,
      Debut = debut
    )
}
