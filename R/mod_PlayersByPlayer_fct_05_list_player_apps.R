get_match_basics <- function() {
  results_dataset %>%
    dplyr::select(
      game_date,
      opposition,
      venue,
      score,
      outcome,
      generic_comp
    )
  }

output_player_apps <- function(inp_player_name) {
  player_apps %>%
    dplyr::filter(
      menu_name == inp_player_name
    ) %>%
    dplyr::left_join(
      get_match_basics(),
      by = "game_date"
    ) %>%
    dplyr::arrange(
      game_date
    ) %>%
    dplyr::mutate(
      app_no = dplyr::row_number(),
      game_date = as.Date(game_date, format = '%d-%m-%Y'),
      game_date = format(game_date, "%d/%m/%Y"),
      role = stringr::str_to_title(role)
    ) %>%
    dplyr::select(
      app_no,
      season,
      game_date,
      opposition,
      venue,
      score,
      outcome,
      generic_comp,
      role,
      shirt_no,
      mins_played,
      goals_scored,
      yellow_cards,
      red_cards
      ) %>%
    dplyr::rename(
      "App No" = app_no,
      Season = season,
      Date = game_date,
      Shirt = shirt_no,
      Opposition = opposition,
      Venue = venue,
      Outcome = outcome,
      Score = score,
      Comp = generic_comp,
      Role = role,
      Mins = mins_played,
      Goals = goals_scored,
      YC = yellow_cards,
      RC = red_cards
    )
}


