output_plr_comps_reactable <- function(inp_player_name) {
  df <- player_apps %>%
    dplyr::filter(
      menu_name == inp_player_name
    ) %>%
    dplyr::left_join(
      results_dataset %>% dplyr::select(game_date, generic_comp),
      by = "game_date"
    ) %>%
    tidyr::replace_na(
      list(
        goals_scored = 0,
        mins_played = 0,
        yellow_cards = 0,
        red_cards = 0,
        mins_played = 0
      )
    ) %>%
    dplyr::mutate(
      generic_comp = factor(
        generic_comp,
        levels = c(
          "Football League",
          "Non-League",
          "War League",
          "FA Cup",
          "League Cup",
          "Associate Members' Cup",
          "Full Members' Cup",
          "Anglo-Italian Cup",
          "FA Trophy"
        )
      )
    ) %>%
    dplyr::group_by(
      generic_comp
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      yellow_cards = sum(yellow_cards),
      red_cards = sum(red_cards),
      mins_played = sum(mins_played)
    ) %>%
    dplyr::mutate(
      total_apps = starts + sub_apps
    ) %>%
    dplyr::select(
      generic_comp,
      total_apps,
      starts,
      sub_apps,
      mins_played,
      goals,
      yellow_cards,
      red_cards
    )

  reactable::reactable(
    data = df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    fullWidth = TRUE,
    resizable = TRUE,
    columns = list(
      generic_comp = reactable::colDef(
        name = "Competition",
        minWidth = 101,
        footer = "Total"
      ),
      total_apps = reactable::colDef(
        name = "Apps",
        footer = function(values) sprintf("%d", sum(values))
      ),
      starts = reactable::colDef(
        name = "Starts",
        footer = function(values) sprintf("%d", sum(values))
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
        footer = function(values) sprintf("%d", sum(values))
      ),
      goals = reactable::colDef(
        name = "⚽️",
        footer = function(values) sprintf("%d", sum(values))
      ),
      mins_played = reactable::colDef(
        name = "⏱️", align = "right",
        format = reactable::colFormat(digits = 0, separators = TRUE),
        footer = function(values) prettyNum(sprintf("%d", sum(values)), big.mark = ",", preserve.width = "none")
      ),
      yellow_cards = reactable::colDef(
        name = "🟨",
        footer = function(values) sprintf("%d", sum(values))
      ),
      red_cards = reactable::colDef(
        name = "🟥",
        footer = function(values) sprintf("%d", sum(values))
      )
    )
  )
}
