get_mgr_h2h_summary <- function(selected_manager) {

  df <- results_dataset %>%
    dplyr::filter(
      manager == selected_manager
    ) %>%
    dplyr::group_by(
      opposition
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA,
      lge_pts = ifelse(
        sum(game_type == "League") != 0,
        sum(outcome == "W" & game_type == "League") * 3 + sum(outcome == "D" & game_type == "League"),
        NA
      ),
      lge_ppg = lge_pts / sum(game_type == "League"),
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      dplyr::desc(P),
      dplyr::desc(lge_pts),
      dplyr::desc(GD),
      dplyr::desc(GF),
      opposition
    )

  reactable::reactable(
    data = df,
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
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(df$opposition)),
    columns = list(
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 180,
        cell = function(value) {
          club_and_crest(value)
        }
      ),
      lge_pts = reactable::colDef(
        name = "League Points",
        cell = function(value) {
          ifelse(!is.na(value), value, '-')
        }
      ),
      lge_ppg = reactable::colDef(
        name = "League PPG",
        format = reactable::colFormat(
          digits = 2
        ),
        cell = function(value) {
          ifelse(!is.na(value), round(value, 2), '-')
        }
      )
    ),
  )
}
