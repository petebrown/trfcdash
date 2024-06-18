get_player_info <- function(pl_name) {
  player_apps %>%
    dplyr::select(
      menu_name,
      player_name,
      player_dob
    ) %>%
    dplyr::mutate(
      player_dob = as.Date(player_dob, format = '%d-%m-%Y'),
      player_dob = format(player_dob, "%d/%m/%Y")
    ) %>%
    dplyr::filter(
      menu_name == pl_name
    ) %>%
    unique()
}

get_player_name <- function(pl_name) {
  get_player_info(pl_name)$player_name
}

get_player_dob <- function(pl_name) {
  get_player_info(pl_name)$player_dob
}

output_player_summary <- function(inp_player_name) {
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
      total_apps = starts + sub_apps
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
      "Yellow cards" = yellow_cards,
      "Red cards" = red_cards
    ) %>%
    reactable::reactable(
      columns = list(
        mins_played = reactable::colDef(
          name = "Mins played",
          format = reactable::colFormat(
            separators = TRUE
          )
        ),
        debut = reactable::colDef(
          name = "Debut",
          align = "right",
          format = reactable::colFormat(
            date = TRUE,
            locales = 'en-GB'
          )
        )
      )
    )
}
