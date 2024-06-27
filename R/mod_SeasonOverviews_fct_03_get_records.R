get_season_records <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {
  min_year <- year_range[1]
  max_year <- year_range[2]

  min_game_no <- game_range[1]
  max_game_no <- game_range[2]

  df <- results_dataset %>%
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
    dplyr::arrange(game_date) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    ) %>%
    dplyr::filter(
      game_no >= min_game_no,
      game_no <= max_game_no
    ) %>%
    generate_record()

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
      minWidth = 60,
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    defaultSorted = "season",
    columns = list(
      season = reactable::colDef(
        name = "Season",
        width = 75
      ),
      GD = reactable::colDef(
        show = TRUE,
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          sprintf("%+3d", value)
        }
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      ),
      Pts = reactable::colDef(
        name = "Lge Pts"
      ),
      PPG = reactable::colDef(
        name = "Lge PPG",
        format = reactable::colFormat(
          digits = 2
        )
      )
    )
  )
}
