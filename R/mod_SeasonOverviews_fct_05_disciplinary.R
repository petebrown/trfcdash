get_season_discipline <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {

  man_res_ssns <- min(results_dataset[results_dataset$season == max(results_dataset$season), ]$game_date)

  min_year <- year_range[1]
  max_year <- year_range[2]

  min_game_no <- game_range[1]
  max_game_no <- game_range[2]

  res <- results_dataset %>%
    dplyr::filter(
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    ) %>%
    dplyr::filter(
      game_no >= min_game_no,
      game_no <= max_game_no
    ) %>%
    dplyr::ungroup()

  df <- res %>%
    dplyr::left_join(
      player_apps %>% dplyr::select(!game_no),
      by = c("season", "game_date")
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      yc = sum(yellow_cards, na.rm = TRUE),
      rc = sum(red_cards, na.rm = TRUE),
      total = yc + rc
    ) %>%
    dplyr::arrange(
      dplyr::desc(total),
      dplyr::desc(rc),
      dplyr::desc(season)
    )

  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    defaultSorted = c("total"),
    columns = list(
      season = reactable::colDef(
        name = "Season"
      ),
      yc = reactable::colDef(
        name = "Yellows"
      ),
      rc = reactable::colDef(
        name = "Reds"
      ),
      total = reactable::colDef(
        name = "Total"
      )
    )
  )


}
