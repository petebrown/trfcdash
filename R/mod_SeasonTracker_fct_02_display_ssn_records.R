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
    defaultColDef = reactable::colDef(
      minWidth = 60
    ),
    columns = list(
      Season = reactable::colDef(
        name = "Season",
        minWidth = 90
      ),
      P = reactable::colDef(
        minWidth = 55
      ),
      W = reactable::colDef(
        minWidth = 55
      ),
      D = reactable::colDef(
        minWidth = 55
      ),
      L = reactable::colDef(
        minWidth = 55
      ),
      GD = reactable::colDef(
        show = TRUE,
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          sprintf("%+3d", value)
        }
      ),
      PPG = reactable::colDef(
        minWidth = 66
      )
    )
  )

  return (tab)
}
