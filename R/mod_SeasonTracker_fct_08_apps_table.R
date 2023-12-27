output_app_table <- function(selected_season) {

  df <- player_apps %>%
    dplyr::filter(
      season %in% selected_season
    ) %>%
    dplyr::left_join(
      results_dataset,
      by = c(
        "season",
        "game_date",
        "game_no"
      )
    ) %>%
    dplyr::mutate(
      generic_comp = dplyr::case_when(
        game_type == "League" ~ "League",
        game_type == "Cup" & !generic_comp %in% c("FA Cup", "League Cup") ~ "Other",
        TRUE ~ generic_comp
      ),
      generic_comp = factor(
        generic_comp,
        levels = c("League", "FA Cup", "League Cup", "Other")
      )
    ) %>%
    dplyr::group_by(
      season,
      menu_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      apps = dplyr::n(),
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      yellows = sum(yellow_cards),
      reds = sum(red_cards),
      mins_played = sum(mins_played),
      mins_per_goal = mins_played / goals,
      games_per_goal = mins_per_goal / 90,
      .groups = "drop"
    ) %>%
    dplyr::select(
      -season
    )

  reactable::reactable(
    data = df,
    defaultPageSize = length(df$menu_name),
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      sortNALast = TRUE
    ),
    groupBy = c("menu_name"),
    compact = TRUE,
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        defaultSortOrder = "asc",
        minWidth = 200,
        grouped = reactable::JS("function(cellInfo) {
            return cellInfo.value
          }")
      ),
      generic_comp = reactable::colDef(
        name = "",
        defaultSortOrder = "asc"
      ),
      apps = reactable::colDef(
        name = "Total Apps",
        aggregate = "sum",
        align = "center"
      ),
      starts = reactable::colDef(
        name = "Starts",
        aggregate = "sum",
        align = "center"
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
        aggregate = "sum",
        align = "center"
      ),
      goals = reactable::colDef(
        name = "Goals",
        aggregate = "sum",
        align = "center"
      ),
      yellows = reactable::colDef(
        name = "Yellow Cards",
        aggregate = "sum",
        align = "center"
      ),
      reds = reactable::colDef(
        name = "Red Cards",
        aggregate = "sum",
        align = "center"
      ),
      mins_played = reactable::colDef(
        name = "Mins Played",
        aggregate = "sum",
        format = reactable::colFormat(
          separators = TRUE
        ),
        align = "right"
      ),
      mins_per_goal = reactable::colDef(
        name = "Mins per Goal",
        defaultSortOrder = "asc",
        align = "right",
        aggregate = reactable::JS("function(values, rows) {
            let mins_played = 0
            let goals = 0
            rows.forEach(function(row) {
              mins_played += row['mins_played']
              goals += row['goals']
            })
            let mins_per_goal = mins_played / goals

            if (isFinite(mins_per_goal)) {
              return Number(mins_per_goal.toFixed(2))
            }
          }"),
        cell = function(value) {
          if (!is.infinite(value)) {
            return (as.integer(round(value, 2)))
          } else {
            return ('-')
          }
        }
      ),
      games_per_goal = reactable::colDef(
        name = "Games per Goal",
        defaultSortOrder = "asc",
        align = "right",
        aggregate = reactable::JS("function(values, rows) {
            let mins_played = 0
            let goals = 0
            rows.forEach(function(row) {
              mins_played += row['mins_played']
              goals += row['goals']
            })
            let games_per_goal = (mins_played / goals) / 90

            if (isFinite(games_per_goal)) {
              return Number(games_per_goal.toFixed(2))
            }
          }"),
        cell = function(value) {
          if (!is.infinite(value)) {
            return (round(value, 2))
          } else {
            return ('-')
          }
        }
      )
    )
  )
}
