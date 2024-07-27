output_pl_summary_by_tier <- function(inp_player_name) {
  df <- player_apps %>%
    dplyr::filter(
      menu_name == inp_player_name
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
    dplyr::left_join(
      results_dataset,
      by = c("game_date", "game_no", "season")
    ) %>%
    dplyr::filter(
      game_type == "League"
    ) %>%
    dplyr::group_by(
      league_tier,
      season
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      mins_played = sum(mins_played),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      Goals = sum(goals_scored),
      win_pc = sum(role == 'starter' & outcome == 'W') / sum(role == 'starter'),
      winning_starts = sum(role == 'starter' & outcome == 'W')
    ) %>%
    dplyr::mutate(
      league_tier = dplyr::case_match(
        league_tier,
        2 ~ "2 - Championship",
        3 ~ "3 - League One",
        4 ~ "4 - League Two",
        5 ~ "5 - National League"
      ),
      mins_per_gl = dplyr::case_when(
        mins_played / Goals != Inf ~ mins_played / Goals,
        TRUE ~ NA
      ),
      games_per_gl = mins_per_gl / 90
    ) %>%
    dplyr::arrange(
      league_tier
    ) %>%
    dplyr::select(
      league_tier,
      season,
      P,
      starts,
      sub_apps,
      W,
      D,
      L,
      win_pc,
      mins_played,
      Goals,
      mins_per_gl,
      games_per_gl,
      winning_starts
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
      vAlign = "top",
      headerClass = "bar-sort-header",
      aggregate = "sum"
    ),
    showSortIcon = FALSE,
    groupBy = c("league_tier"),
    columns = list(
      league_tier = reactable::colDef(
        name = "Tier",
        minWidth = 185,
        format = reactable::colFormat(
          prefix = "Tier "
        ),
        grouped = reactable::JS("function(cellInfo) {
          return cellInfo.value
        }"),
        footer = NULL
      ),
      season = reactable::colDef(
        name = "Season",
        minWidth = 75,
        aggregate = reactable::JS("function(values, rows) {
          let seasons = 0

          rows.forEach(function(row) {
            seasons += 1
          })
          return 'All (' + seasons + ')'
        }"),
        footer = "Total"
      ),
      P = reactable::colDef(
        name = "Apps",
        footer = function(values) {
          sum(values)
        }
      ),
      starts = reactable::colDef(
        name = "Starts",
        footer = function(values) {
          sum(values)
        }
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
        format = reactable::colFormat(
          prefix = "(",
          suffix = ")"
        ),
        footer = function(values) {
          total = sum(values)
          paste0("(", total, ")")
        }
      ),
      W = reactable::colDef(
        footer = function(values) {
          sum(values)
        }
      ),
      D = reactable::colDef(
        footer = function(values) {
          sum(values)
        }
      ),
      L = reactable::colDef(
        footer = function(values) {
          sum(values)
        }
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        na = '-',
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        ),
        aggregate = reactable::JS("function(values, rows) {
          let games_played = 0
          let wins = 0
          rows.forEach(function(row) {
            games_played += row['starts']
            wins += row['winning_starts']
          })
          if (games_played === 0) {
            return '-'
          } else {
            return wins / games_played
          }
        }"),
        footer = reactable::JS("function(state) {
          console.log(state.data);
          let data = state.data;
          let starts = 0;
          let wins = 0;

          data.forEach(function(row) {
            starts += row.starts;
            wins += row.winning_starts;
          });

          if (starts === 0) {
            return '-';
          } else {
            let win_pc = wins / starts;
            win_pc = Math.round(win_pc * 100).toFixed(1);
            return win_pc + '%';
          }
        }")
      ),
      mins_played = reactable::colDef(
        name = "Mins played",
        format = reactable::colFormat(
          separators = TRUE
        ),
        footer = reactable::JS("function(state) {
          let data = state.data;
          let mins_played = 0;

          data.forEach(function(row) {
            mins_played += row.mins_played;
          });

          return mins_played.toLocaleString();
        }")
      ),
      mins_per_gl = reactable::colDef(
        name = "Mins per goal",
        na = '-',
        format = reactable::colFormat(
          digits = 0
        ),
        aggregate = reactable::JS("function(values, rows) {
          let mins_played = 0
          let goals = 0
          rows.forEach(function(row) {
            mins_played += row['mins_played']
            goals += row['Goals']
          })
          if (goals === 0) {
            return '-';
          } else {
          let mins_per_goal = mins_played / goals;
          return mins_per_goal.toFixed(1);
          }
        }"),
        footer = reactable::JS("function(state) {
          console.log(state.data);
          let data = state.data;
          let mins_played = 0;
          let goals = 0;

          data.forEach(function(row) {
            mins_played += row.mins_played;
            goals += row.Goals;
          });

          if (goals === 0) {
            return '-';
          } else {
            let mins_per_goal = mins_played / goals;
            return mins_per_goal.toFixed(1);
          }
        }")
      ),
      Goals = reactable::colDef(
        name = "️Goals",
        footer = function(values) {
          sum(values)
        }
      ),
      games_per_gl = reactable::colDef(
        name = "Games per goal",
        na = '-',
        format = reactable::colFormat(
          digits = 1
        ),
        aggregate = reactable::JS("function(values, rows) {
          let mins_played = 0
          let goals = 0
          rows.forEach(function(row) {
            mins_played += row['mins_played']
            goals += row['Goals']
          })
          if (goals === 0) {
            return '-'
          } else {
            return mins_played / goals / 90
          }
        }"),
        footer = reactable::JS("function(state) {
          console.log(state.data);
          let data = state.data;
          let mins_played = 0;
          let goals = 0;

          data.forEach(function(row) {
            mins_played += row.mins_played;
            goals += row.Goals;
          });

          if (goals === 0) {
            return '-';
          } else {
          let games_per_goal = mins_played / goals / 90;
          return games_per_goal.toFixed(1);
          }
        }")
      ),
      winning_starts = reactable::colDef(
        show = FALSE
      )
    )
  )
}
