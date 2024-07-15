get_eos_table <- function(selected_seasons) {
  res <- filter_ssn_results(selected_seasons) %>%
    dplyr::select(game_date, league_tier)

  final_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::mutate(
      GD = GF - GA
    ) %>%
    dplyr::relocate(
      GD,
      .after = GA
    ) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    ) %>%
    dplyr::left_join(
      res,
      by = "game_date"
    )
}

get_lge_tables <- function(selected_seasons) {
  res <- filter_ssn_results(selected_seasons) %>%
    dplyr::select(game_date, league_tier)

  lge_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    ) %>%
    dplyr::left_join(
      res,
      by = "game_date"
    )
}
