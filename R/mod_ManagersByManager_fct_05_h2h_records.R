get_mgr_h2h_summary <- function(selected_manager) {

  results_dataset %>%
    dplyr::filter(
      manager == selected_manager
    ) %>%
    dplyr::group_by(
      opposition
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA,
      lge_pts = sum(outcome == "W" & game_type == "League") * 3 + sum(outcome == "D" & game_type == "League"),
      lge_ppg = lge_pts / sum(game_type == "League"),
      .groups = "drop"
    )
}
