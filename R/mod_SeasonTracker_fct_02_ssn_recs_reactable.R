ssn_recs_reactable <- function(selected_seasons, selected_venue, pens_as_draw = "Yes") {
  df <- results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons,
      venue %in% selected_venue
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::group_by(
      season,
      generic_comp
    ) %>%
    summarise_results()

  reactable::reactable(
    data = df,
    groupBy = c("season"),
    striped = TRUE,
    defaultPageSize = length(df$season),
    searchable = TRUE,
    rowStyle = function() {
      list(
        `font-weight` = 300,
        `background` = "#f8fdff91"
      )
    },
    columns = list(
      season = reactable::colDef(
        name = "Season",
        align = "left",
        sticky = "left",
        minWidth = 50,
        grouped = reactable::JS("function(cellInfo) {
            return cellInfo.value
          }")
      ),
      generic_comp = reactable::colDef(
        name = "Competition",
        align = "left",
        sticky = "left",
        minWidth = 80,
        aggregate = reactable::JS("function(values, rows) {
            let comps = 0

            rows.forEach(function(row) {
              comps += 1
            })
            return 'All competitions (' + comps + ')'
          }"),
      ),
      P = reactable::colDef(
        name = "P",
        aggregate = "sum",
        align = "center",
        minWidth = 50,
        footer = js_total_col()
      ),
      W = reactable::colDef(
        name = "W",
        aggregate = "sum",
        align = "center",
        minWidth = 30,
        footer = js_total_col()
      ),
      D = reactable::colDef(
        name = "D",
        aggregate = "sum",
        align = "center",
        minWidth = 30,
        footer = js_total_col()
      ),
      L = reactable::colDef(
        name = "L",
        aggregate = "sum",
        align = "center",
        minWidth = 30,
        footer = js_total_col()
      ),
      GF = reactable::colDef(
        name = "GF",
        aggregate = "sum",
        align = "center",
        minWidth = 30,
        footer = js_total_col()
      ),
      GA = reactable::colDef(
        name = "GA",
        aggregate = "sum",
        align = "center",
        minWidth = 30,
        footer = js_total_col()
      ),
      GD = reactable::colDef(
        name = "GD",
        aggregate = "sum",
        align = "center",
        minWidth = 30,
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
        cell = function(value) {
          # Format as percentages with 1 decimal place
          value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
          bar_chart(
            value,
            width = value,
            fill = "lightblue",
            background = "#F2F2F2"
          )
        }
      )
    )
  )
}
