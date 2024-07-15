output_all_mgr_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games, inc_caretakers) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome,
      )
    ) %>%
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
    dplyr::group_by(
      manager
    ) %>%
    generate_streaks(drop_games_played = FALSE) %>%
    dplyr::filter(
      P >= min_games,
    ) %>%
    dplyr::arrange(
      dplyr::desc(wins)
    ) %>%
    dplyr::select(
      manager,
      wins,
      unbeaten,
      defeats,
      winless,
      draws,
      clean_sheets,
      wins_to_nil
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
    defaultSorted = list("wins" = "desc"),
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
      mgr_name = reactable::colDef(
        name = "",
        vAlign = "center"
      ),
      wins = reactable::colDef(
        name = "Wins",
        vAlign = "center"
      ),
      unbeaten = reactable::colDef(
        name = "Unbeaten",
        vAlign = "center"
      ),
      defeats = reactable::colDef(
        name = "Defeats",
        vAlign = "center"
      ),
      winless = reactable::colDef(
        name = "Winless",
        vAlign = "center"
      ),
      draws = reactable::colDef(
        name = "Draws",
        vAlign = "center"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets",
        vAlign = "center"
      ),
      wins_to_nil = reactable::colDef(
        name = "Wins to nil",
        vAlign = "center"
      ),
      mgr_headshot = reactable::colDef(
        show = FALSE
      )
    )
  )

  return(output_tab)

}
