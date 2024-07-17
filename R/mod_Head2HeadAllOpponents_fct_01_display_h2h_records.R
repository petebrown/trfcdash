output_h2h_records <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {
  df <- results_dataset %>%
    dplyr::filter(
      ssn_year >= year_range[1],
      ssn_year <= year_range[2]
    ) %>%
    dplyr::filter(
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
    dplyr::group_by(
      opposition
    ) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA
    ) %>%
    dplyr::mutate(
      win_pc = (W / P)
    ) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(win_pc),
      P,
      opposition
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
    defaultSorted = "win_pc",
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$opposition)),
    defaultColDef = reactable::colDef(
      vAlign = "center",
      align = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    columns = list(
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 205,
        align = "left",
        defaultSortOrder = "asc",
        cell = reactable::JS("function(cellInfo, state) {
          const { crests } = state.meta;

          let opponent = cellInfo.value;
          let img_src = crests[opponent];

          img = `<img src='${img_src}' style='height:32px; margin:2px;' alt='${opponent}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:space-between; align-items:center; width:36px;'>${img}</div>
            <div style='display:flex; text-align:left; margin:auto 0 auto 6.5px;'>${opponent}</div>
          </div>
          `
        }"),
        html = TRUE
      ),
      P = reactable::colDef(
      ),
      W = reactable::colDef(
      ),
      D = reactable::colDef(
      ),
      L = reactable::colDef(
      ),
      GF = reactable::colDef(
      ),
      GA = reactable::colDef(
      ),
      GD = reactable::colDef(
        vAlign = "center",
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          sprintf("%+3d", value)
        }
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        align = "right",
        vAlign = "center",
        minWidth = 180,
        defaultSortOrder = "desc",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let bar_width = value * 100;
          let bar_fill = 'lightblue';
          let bar_background = '#F2F2F2';
          let display_value = (value * 100).toFixed(1) + '%';

          return `
            <div style='display:flex; align-items:center;'>
              <div style='flex-grow:1; margin-left:3px; margin-right:10px; background:${bar_background}; border-style:solid; border-color:slategrey; border-width:thin'>
                <div style='background:${bar_fill}; width:${bar_width}%; height:1.5rem;'></div>
              </div>
              <div style='width:45px'>${display_value}</div>
            </div>
          `;
        }"),
        html = TRUE
      )
    )
  )
}
