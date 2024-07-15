output_h2h_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

  df <- results_dataset %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4))
    ) %>%
    dplyr::filter(
      ssn_year >= year_range[1],
      ssn_year <= year_range[2],
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
    dplyr::arrange(
      season,
      game_no
    ) %>%
    dplyr::group_by(
      opposition
    ) %>%
    generate_streaks(drop_games_played = FALSE) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(wins),
      dplyr::desc(P)
    )

  crest_list <- as.list(
    clubs_crests %>%
      dplyr::select(club, file_path) %>%
      dplyr::pull(file_path) %>%
      purrr::set_names(clubs_crests %>% dplyr::pull(club))
  )

  reactable::reactable(
    data = df,
    meta = list(
      crests = crest_list
    ),
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "wins",
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$opposition)),
    defaultColDef = reactable::colDef(
      align = "center",
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    columns = c(
      list(
        opposition = reactable::colDef(
          name = "Opposition",
          minWidth = 175,
          defaultSortOrder = "asc",
          cell = reactable::JS("function(cellInfo, state) {
          const { crests } = state.meta;

          let opponent = cellInfo.value;
          let img_src = crests[opponent];

          img = `<img src='${img_src}' style='height:32px; margin:2px;' alt='${opponent}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:40px;'>${img}</div>
            <div style='display:flex; text-align:left; margin:6.4px;'>${opponent}</div>
          </div>
          `
        }"),
          html = TRUE
        ),
        P = reactable::colDef(
          show = FALSE
        )
      ),
      format_streak_cols()
    )
  )
}
