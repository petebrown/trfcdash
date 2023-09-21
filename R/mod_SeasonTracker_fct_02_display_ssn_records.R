output_ssn_records <- function(selected_seasons, selected_venue) {
  df <- filter_ssn_results(selected_seasons) %>%
    dplyr::filter(
      game_type == "League",
      venue %in% selected_venue
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA,
      Pts = (W * 3) + D,
      PPG = round(Pts / P, 2)
    ) %>%
    dplyr::rename(Season = season)

  tab <- reactable::reactable(
    data = df,
    striped = TRUE,
    columns = list(
      GD = reactable::colDef(
        show = TRUE,
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          sprintf("%+3d", value)
        }
      )
    )
  )

  return (tab)
}
