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
    generate_record() %>%
    dplyr::arrange(
      season,
      dplyr::desc(P)
    ) %>%
    dplyr::relocate(
      win_pc,
      .after = PPG
    )

  reactable::reactable(
    data = df,
    groupBy = c("season"),
    striped = FALSE,
    defaultPageSize = length(df$season),
    compact = TRUE,
    style = list(
      fontSize = "smaller",
      fontWeight = 400,
      color = "black"
    ),
    rowStyle = function() {
      list(
        # fontSize = "smaller",
        fontWeight = 300,
        color = "black"
      )
    },
    columns = list(
      season = reactable::colDef(
        name = "Season",
        align = "left",
        sticky = "left",
        minWidth = 90,
        grouped = reactable::JS("function(cellInfo) {
            return cellInfo.value
          }")
      ),
      generic_comp = reactable::colDef(
        name = "Competition",
        align = "left",
        sticky = "left",
        minWidth = 110,
        aggregate = reactable::JS("function(values, rows) {
            let comps = 0

            rows.forEach(function(row) {
              comps += 1
            })
            return 'All comps (' + comps + ')'
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
        minWidth = 35,
        footer = js_total_col()
      ),
      D = reactable::colDef(
        name = "D",
        aggregate = "sum",
        align = "center",
        minWidth = 35,
        footer = js_total_col()
      ),
      L = reactable::colDef(
        name = "L",
        aggregate = "sum",
        align = "center",
        minWidth = 35,
        footer = js_total_col()
      ),
      GF = reactable::colDef(
        name = "GF",
        aggregate = "sum",
        align = "center",
        minWidth = 50,
        footer = js_total_col()
      ),
      GA = reactable::colDef(
        name = "GA",
        aggregate = "sum",
        align = "center",
        minWidth = 50,
        footer = js_total_col()
      ),
      GD = reactable::colDef(
        name = "GD",
        aggregate = "sum",
        align = "center",
        minWidth = 50,
        footer = js_total_col()
      ),
      Pts = reactable::colDef(
        name = "Pts",
        aggregate = "sum",
        align = "right",
        minWidth = 50,
        footer = js_total_col()
      ),
      PPG = reactable::colDef(
        name = "PPG",
        aggregate = "sum",
        align = "right",
        minWidth = 50,
        format = reactable::colFormat(
          digits = 2
        )
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        minWidth = 70,
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
        # cell = function(value) {
        #   # Format as percentages with 1 decimal place
        #   value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
        #   bar_chart(
        #     value,
        #     width = value,
        #     fill = "lightblue",
        #     background = "#F2F2F2"
        #   )
        # }
      )
    )
  )
}
