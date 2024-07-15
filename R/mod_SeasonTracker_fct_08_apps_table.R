output_app_table <- function(selected_season, inc_cup_games, pens_as_draw, min_starts, show_images='Yes') {

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

  # plr_positions <- as.list(
  #   player_positions %>%
  #     dplyr::select(pl_index, position) %>%
  #     dplyr::pull(position) %>%
  #     purrr::set_names(player_positions %>% dplyr::pull(pl_index))
  # )
  # plr_headshots <- as.list(
  #   player_imgs %>%
  #     dplyr::select(pl_index, headshot_file_path) %>%
  #     dplyr::pull(headshot_file_path) %>%
  #     purrr::set_names(player_imgs %>% dplyr::pull(pl_index))
  # )


  reactable::reactable(
    data = df,
    meta = list(
      plr_positions = meta_plr_positions,
      plr_headshots = meta_plr_headshots
    ),
    class = if (show_images == "Yes") {
      "apps_table_with_images"
    },
    defaultPageSize = length(df$menu_name),
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      minWidth = 70,
      sortNALast = TRUE,
      vAlign = "center",
      headerVAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
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
        class = "player-name",
        defaultSortOrder = "asc",
        minWidth = 185,
        grouped = if (show_images=='Yes') {
          reactable::JS("function(cellInfo, state) {
            const player = cellInfo.value;
            const plr_name = player.split(' (b.')[0];
            const plr_pos = state.meta.plr_positions[player];
            const img_src = state.meta.plr_headshots[player];

            img = `<img src='${img_src}' style='height:45px; margin:auto;' alt='${plr_name}'>`;

            return `
            <div style='display: flex'>
              <div style='display:flex; justify-content:center; width:50px;'>${img}</div>
                <div style='text-align:left; margin:auto 10px; line-height:1rem;'>
                  ${plr_name}
                  <br>
                  <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>${plr_pos}</span>
                </div>
              </div>
              `
          }")
        } else {
          reactable::JS("function(cellInfo) {
            return cellInfo.value;
          }")
        },
        cell = reactable::JS("function(cellInfo, state) {
            const player = cellInfo.value;
            const plr_name = player.split(' (b.')[0];
            const plr_pos = state.meta.plr_positions[player];
            const img_src = state.meta.plr_headshots[player];

            img = `<img src='${img_src}' style='height:45px; margin:auto;' alt='${plr_name}'>`;

            return `
            <div style='display: flex'>
              <div style='display:flex; justify-content:center; width:50px;'>${img}</div>
                <div style='text-align:left; margin:auto 10px; line-height:1rem;'>
                  ${plr_name}
                  <br>
                  <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>${plr_pos}</span>
                </div>
              </div>
              `
          }"),
        html = TRUE
      ),
      generic_comp = reactable::colDef(
        name = "",
        defaultSortOrder = "asc",
        width = 110
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
        sortNALast = TRUE,
        align = "right",
        format = reactable::colFormat(
          separators = TRUE,
          digits = 0
        ),
        aggregate = if (inc_cup_games == "Yes") {
          reactable::JS("function(values, rows) {
            let mins_played = 0;
            let goals = 0;
            rows.forEach(function(row) {
              mins_played += row['mins_played'];
              goals += row['goals'];
            })
            let mins_per_goal = mins_played / goals;

            if (isFinite(mins_per_goal)) {
              return parseFloat(mins_per_goal.toFixed(0));
            }
          }")
        },
        aggregated = reactable::JS("function(cellInfo, state) {
          if (isFinite(cellInfo.value)) {
            return cellInfo.value
          } else {
            return '-'
          }
        }"),
        cell = function(value) {
          if (!is.infinite(value) && !is.na(value)) {
            return (round(value, 0))
          } else {
            return ('-')
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
        aggregated = reactable::JS("function(cellInfo, state) {
          if (isFinite(cellInfo.value)) {
            return cellInfo.value
          } else {
            return '-'
          }
        }"),
        cell = function(value) {
          if (!is.infinite(value) && !is.na(value)) {
            return (round(value, 2))
          } else {
            return ('-')
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
        aggregated = reactable::JS("function(cellInfo, state) {
          if (isFinite(cellInfo.value)) {
            let pc = cellInfo.value * 100;
            return pc.toFixed(1) + '%'
          } else {
            return '-'
          }
        }"),
        cell = function(value) {
          if (!is.infinite(value) && !is.na(value)) {
            return (paste0(round(value * 100, 1), '%'))
          } else {
            return ('-')
          }
        }
      )
    )
  )
}
