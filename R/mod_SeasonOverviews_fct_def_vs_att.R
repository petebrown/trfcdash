get_def_vs_att <- function(selected_seasons) {
  results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarize(
      p = dplyr::n(),
      clean_sheets = sum(goals_against == 0),
      cs_pc = clean_sheets / p,
      av_ga = mean(goals_against),
      scored = sum(goals_for > 0),
      scored_pc = scored / p,
      av_gf = mean(goals_for),
      blank = sum(goals_for == 0),
      blank_pc = blank / p,
      .groups = "drop"
    )
}
