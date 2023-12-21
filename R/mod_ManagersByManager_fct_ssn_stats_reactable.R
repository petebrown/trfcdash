get_mgr_summary_by_ssn_reactable <- function(selected_manager) {

  df <- results_dataset %>%
    dplyr::filter(
      manager == selected_manager
    ) %>%
    dplyr::group_by(
      season,
      competition
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(goals_for) - sum(goals_against),
      win_pc = W / P,
      .groups = "drop"
    ) %>%
    dplyr::arrange(
      season,
      dplyr::desc(P)
    )


  reactable::reactable(
    data = df,
    elementId = "mgrs_by_season",
    defaultPageSize = length(df$season),
    searchable = TRUE,
    groupBy = c("season"),
    rowStyle = function() {
      list(
        `font-weight` = "lighter",
        `background` = "#f8fdff91"
      )
    },
    columns = list(
      season = reactable::colDef(
        name = "Season",
        align = "left"
      ),
      competition = reactable::colDef(
        name = "Competition",
        align = "left",
        aggregate = "count",
        format = list(
          aggregated = reactable::colFormat(
            suffix = " competitions"
          )
        )
      ),
      P = reactable::colDef(
        name = "P",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      W = reactable::colDef(
        name = "W",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      D = reactable::colDef(
        name = "D",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      L = reactable::colDef(
        name = "L",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      GF = reactable::colDef(
        name = "GF",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      GA = reactable::colDef(
        name = "GA",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      GD = reactable::colDef(
        name = "GD",
        aggregate = "sum",
        align = "center",
        footer = js_total_col()
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        align = "right",
        aggregate = reactable::JS("function(values, rows) {
          let games_played = 0
          let wins = 0
          rows.forEach(function(row) {
            games_played += row['P']
            wins += row['W']
          })
          return wins / games_played
        }"),
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        ),
        footer = reactable::JS("function(colInfo) {
          const formatter = Intl.NumberFormat('en-US', {
            style: 'percent',
            maximumFractionDigits: 1,
          })

          var games_played = 0
          var wins = 0

          colInfo.data.forEach(function(row) {
            games_played += row['P']
            wins += row['W']
          })

          var win_pc = wins / games_played

          return formatter.format(win_pc)
        }")
      )
    )
  )
}
