get_biggest_defeats_by_opp <- function(df, biggest_diff=1) {

  df <- df %>%
    dplyr::filter(
      outcome == "L",
      goal_diff <= -biggest_diff
    ) %>%
    dplyr::arrange(
      goal_diff,
      dplyr::desc(goals_against),
      game_date
    ) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      venue,
      opposition,
      score,
      competition,
      attendance,
      manager
    )

  reactable::reactable(
    data = df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(df$season)),
    fullWidth = TRUE,
    compact = TRUE,
    searchable = TRUE,
    borderless = TRUE,
    filterable = FALSE,
    resizable = TRUE,
    rowClass = "results-row",
    columns = list(
      season = reactable::colDef(
        name = "Season",
        minWidth = 100
      ),
      game_no = reactable::colDef(
        name = "Game",
        align = "center",
        minWidth = 65
      ),
      game_date = reactable::colDef(
        name = "Date",
        minWidth = 105,
        format = reactable::colFormat(date = TRUE, locales = "en-GB")
      ),
      venue = reactable::colDef(
        name = "Venue",
        minWidth = 65,
        align = "center"
      ),
      opposition = reactable::colDef(
        name = "Opponent",
        minWidth = 180,
        cell = function(value, index) {
          venue <- df$venue[index]

          if (venue == "H") {
            return (toupper(value))
          } else {
            return (value)
          }
        },
        style = function(value, index) {
          if (df$venue[index] == "H") {
            font_weight = "450"
          } else {
            font_weight = "300"
          }
          list(
            fontWeight = font_weight
          )
        }
      ),
      outcome = reactable::colDef(
        show = FALSE,
        name = "Res",
        align = "center",
        minWidth = 45
      ),
      score = reactable::colDef(
        name = "Score",
        minWidth = 60,
        align = "center"
      ),
      competition = reactable::colDef(
        name = "Competition",
        minWidth = 180,
        cell = function(value) {
          generic_comp_logo(value)
        }
      ),
      attendance = reactable::colDef(
        name = "Att.",
        minWidth = 70,
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      manager = reactable::colDef(
        name = "Manager",
        minWidth = 120,
        align = "right"
      ),
      league_tier = reactable::colDef(
        show = FALSE
      )
    )
  )
}
