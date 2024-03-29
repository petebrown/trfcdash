output_app_table <- function(selected_season, inc_cup_games, pens_as_draw, min_starts) {

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
    )

  if (inc_cup_games == "No") {
    df <- df %>%
      dplyr::filter(game_type == "League")
  }

  df <- df %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      ),
      generic_comp = dplyr::case_when(
        game_type == "League" ~ "League",
        stringr::str_detect(game_type, "League Play") ~ "League Play-Off",
        game_type == "Cup" & !generic_comp %in% c("FA Cup", "League Cup") ~ "Other",
        TRUE ~ generic_comp
      ),
      generic_comp = factor(
        generic_comp,
        levels = c("League", "League Play-Off", "FA Cup", "League Cup", "Other")
      )
    ) %>%
    dplyr::group_by(
      season,
      menu_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      apps = dplyr::n(),
      goals = sum(goals_scored),
      yellows = sum(yellow_cards),
      reds = sum(red_cards),
      mins_played = sum(mins_played),
      mins_per_goal = ifelse(mins_played > 0, mins_played / goals, NA),
      games_per_goal = mins_per_goal / 90,
      winning_starts = sum(role == "starter" & outcome == "W"),
      win_pc = winning_starts / sum(role == "starter"),
      .groups = "drop"
    ) %>%
    dplyr::select(
      -season
    ) %>%
    dplyr::filter(
      starts >= min_starts
    )

  reactable::reactable(
    data = df,
    defaultPageSize = length(df$menu_name),
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      sortNALast = TRUE
    ),
    groupBy = if (inc_cup_games == "Yes") {
      c("menu_name")
    },
    compact = TRUE,
    style = list(
      fontWeight = 400,
      color = "black"
    ),
    rowStyle = if (inc_cup_games == "Yes") {
      function() {
        list(
          fontWeight = 300,
          color = "black"
        )
      }
    },
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        sticky = "left",
        defaultSortOrder = "asc",
        minWidth = 185,
        grouped = reactable::JS("function(cellInfo) {
            return cellInfo.value
          }")
      ),
      generic_comp = reactable::colDef(
        name = "",
        defaultSortOrder = "asc",
        minWidth = 90
      ),
      apps = reactable::colDef(
        name = "Total Apps",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center"
      ),
      starts = reactable::colDef(
        name = "Starts",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center"
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        format = reactable::colFormat(
          prefix = "(",
          suffix = ")"
        ),
        align = "center",
        cell = function(value) {
          parens = stringr::str_glue('({value})')
          return (parens)
        }
      ),
      goals = reactable::colDef(
        name = "Goals",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center"
      ),
      yellows = reactable::colDef(
        name = "Yellow Cards",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center"
      ),
      reds = reactable::colDef(
        name = "Red Cards",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center"
      ),
      mins_played = reactable::colDef(
        name = "Mins Played",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        format = reactable::colFormat(
          separators = TRUE
        ),
        align = "right"
      ),
      mins_per_goal = reactable::colDef(
        name = "Mins per Goal",
        defaultSortOrder = "asc",
        align = "right",
        format = reactable::colFormat(
          separators = TRUE,
          digits = 0
        ),
        na = "-",
        aggregate = if (inc_cup_games == "Yes") {
          reactable::JS("function(values, rows) {
            let mins_played = 0
            let goals = 0
            rows.forEach(function(row) {
              mins_played += row['mins_played']
              goals += row['goals']
            })
            let mins_per_goal = mins_played / goals

            if (isFinite(mins_per_goal)) {
              return Number(mins_per_goal)
            }
          }")
        },
        cell = function(value) {
          if (!is.infinite(value) && !is.na(value)) {
            return (round(value, 0))
          }
        }
      ),
      games_per_goal = reactable::colDef(
        name = "Games per Goal",
        defaultSortOrder = "asc",
        align = "right",
        format = reactable::colFormat(
          digits = 1
        ),
        na = "-",
        aggregate = if (inc_cup_games == "Yes") {
          reactable::JS("function(values, rows) {
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
          }")
        },
        cell = function(value) {
          if (!is.infinite(value) && !is.na(value)) {
            return (round(value, 2))
          }
        }
      ),
      winning_starts = reactable::colDef(
        show = FALSE,
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        }
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        align = "right",
        na = "-",
        aggregate = if (inc_cup_games == "Yes") {
          reactable::JS("function(values, rows) {
            let winning_starts = 0
            let starts = 0
            rows.forEach(function(row) {
              winning_starts += row['winning_starts']
              starts += row['starts']
            })
            let win_pc = winning_starts / starts

            if (isFinite(win_pc)) {
              return Number(win_pc)
            }
          }")
        },
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      )
    )
  )
}
