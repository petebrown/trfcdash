get_teammate_plot <- function(selected_player) {

  # plr_starts <- player_apps %>%
  #   dplyr::left_join(
  #     results_dataset,
  #     by = c(
  #       "season",
  #       "game_date",
  #       "game_no"
  #     )
  #   ) %>%
  #   dplyr::filter(
  #     menu_name == selected_player,
  #     role == "starter",
  #     game_type == "League"
  #   )
  #
  # plot_data <- player_apps %>%
  #   dplyr::left_join(
  #     results_dataset,
  #     by = c(
  #       "season",
  #       "game_date",
  #       "game_no"
  #     )
  #   ) %>%
  #   dplyr::filter(
  #     menu_name != selected_player,
  #     game_date %in% plr_starts$game_date,
  #     role == "starter",
  #     game_type == "League"
  #   ) %>%
  #   dplyr::group_by(
  #     menu_name
  #   ) %>%
  #   dplyr::mutate(
  #     game_pts = dplyr::case_match(
  #       outcome,
  #       "W" ~ 3,
  #       "D" ~ 1,
  #       "L" ~ 0
  #     )
  #   ) %>%
  #   dplyr::summarise(
  #     games = dplyr::n(),
  #     av_pts = mean(game_pts),
  #     win_pc = sum(outcome == "W") / games
  #   )
  #
  # return (plot_data)

}

get_teammate_plot("Davies, Tom")
