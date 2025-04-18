get_players_used <- function (df) {
  df %>%
    dplyr::group_by(
      season,
      menu_name
    ) %>%
    dplyr::summarise(
      n_starts = sum(role == "starter"),
      n_sub = sum(role == "sub"),
      .groups = "drop"
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      starters = sum(n_starts > 0),
      sub_only = sum(n_starts == 0 & n_sub > 0),
      n_players = dplyr::n()
    ) %>%
    dplyr::arrange(
      dplyr::desc(n_players)
    )
}

get_season_debuts <- function(df) {
  df %>%
    dplyr::select(
      game_date,
      season
    ) %>%
    dplyr::inner_join(
      player_debuts,
      by = c("game_date" = "debut_date")
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      n_debuts = dplyr::n()
    )
}

get_season_plr_stats <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {

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
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
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

  n_debutants <- get_season_debuts(res)

  df <- res %>%
    dplyr::inner_join(
      player_apps %>% dplyr::select(!game_no),
      by = c("season", "game_date")
    ) %>%
    get_players_used() %>%
    dplyr::arrange(
      dplyr::desc(n_players),
      dplyr::desc(starters),
      dplyr::desc(season)
    ) %>%
    dplyr::left_join(
      n_debutants,
      by = "season"
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$season)),
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    defaultSorted = c("n_players"),
    columns = list(
      season = reactable::colDef(
        name = "Season",
        width = 75
      ),
      n_players = reactable::colDef(
        name = "Total Players Used"
      ),
      starters = reactable::colDef(
        name = "Unique Starters"
      ),
      sub_only = reactable::colDef(
        name = "Subs Only"
      ),
      n_debuts = reactable::colDef(
        name = "Debutants"
      )
    )
  )


}
