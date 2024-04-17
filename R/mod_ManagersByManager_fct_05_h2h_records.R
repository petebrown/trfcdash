get_mgr_h2h_summary <- function(selected_manager) {

  df <- results_dataset %>%
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
      lge_pts = ifelse(
        sum(game_type == "League") != 0,
        sum(outcome == "W" & game_type == "League") * 3 + sum(outcome == "D" & game_type == "League"),
        NA
      ),
      lge_ppg = lge_pts / sum(game_type == "League"),
      .groups = "drop"
    )

  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    columns = list(
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 180,
        cell = function(value) {
          club_and_crest(value)
        }
      ),
      lge_pts = reactable::colDef(
        name = "League Points"
      ),
      lge_ppg = reactable::colDef(
        name = "League PPG",
        format = reactable::colFormat(
          digits = 2
        )
      )
    ),
  )
}
