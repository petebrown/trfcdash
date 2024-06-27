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
  df <- player_apps %>%
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
      game_no = dplyr::row_number(),
      role = stringr::str_to_title(role)
    ) %>%
    dplyr::select(
      game_no,
      season,
      game_date,
      venue,
      opposition,
      outcome,
      score,
      generic_comp,
      role,
      shirt_no,
      mins_played,
      goals_scored,
      yellow_cards,
      red_cards
    )

  compile_results_table(df, page='player')

}


