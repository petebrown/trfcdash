output_ssn_records <- function(selected_seasons) {
  df <- filter_ssn_results(selected_seasons) %>%
    dplyr::filter(game_type == "League") %>%
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

  return (df)
}
