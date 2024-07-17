output_all_mgr_records <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games, inc_caretakers) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options,
      dplyr::case_when(
        inc_caretakers == "No" ~ mgr_role != "Caretaker",
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::group_by(manager) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(GF - GA),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      win_pc = W / P
    ) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(P),
      manager
    ) %>%
    dplyr::select(
      manager,
      P,
      W,
      D,
      L,
      GF,
      GA,
      GD,
      win_pc
    )

  mgr_imgs <- manager_imgs %>%
    dplyr::select(
      manager_name,
      mgr_headshot = headshot_file_path
    )

  df <- dplyr::left_join(
    df,
    mgr_imgs,
    by = c("manager" = "manager_name")
  )

  output_tab <- reactable::reactable(
    data = df,
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$manager)),
    searchable = TRUE,
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
    defaultSorted = list(
      "P" = "desc"
    ),
    columns = list(
      manager = reactable::colDef(
        name = "",
        width = 200,
        vAlign = "top",
        cell = reactable::JS("function(cellInfo) {
          let img_src = cellInfo.row['mgr_headshot'];
          let manager = cellInfo.value;
          let borderRadius = 50;
          let border = '0.1pt black solid';

          if (manager === 'No manager') {
            borderRadius = 0;
            border = 'none';
          }

          img = `<img src='${img_src}' style='height: 50px; border-radius: ${borderRadius}%; border: ${border}' alt='${manager}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:60px;'>${img}</div>
            <div style='display:flex; margin-left:10px; text-align:left; align-items:center;'>${manager}</div>
          </div>
          `
        }"),
        html = TRUE
      ),
      P = reactable::colDef(
        vAlign = "center",
      ),
      W = reactable::colDef(
        vAlign = "center",
      ),
      D = reactable::colDef(
        vAlign = "center",
      ),
      L = reactable::colDef(
        vAlign = "center",
      ),
      GF = reactable::colDef(
        vAlign = "center",
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      GA = reactable::colDef(
        vAlign = "center",
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      GD = reactable::colDef(
        vAlign = "center",
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          format_gd(value)
        }
      ),
      win_pc = reactable::colDef(
        name = "Win Rate",
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
      ),
      mgr_headshot = reactable::colDef(
        show = FALSE
      )
    )
  )

  return(output_tab)
}
