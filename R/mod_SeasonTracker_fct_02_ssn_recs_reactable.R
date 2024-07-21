ssn_recs_reactable <- function(selected_seasons, selected_venue, inc_cup_games, pens_as_draw) {

  df <- results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons,
      venue %in% selected_venue,
      dplyr::case_when(
        inc_cup_games == "No" ~ game_type == "League",
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::group_by(
      season,
      competition
    ) %>%
    generate_record() %>%
    dplyr::arrange(
      season,
      dplyr::desc(P)
    )

  comp_logos <- spec_comp_logos %>%
    dplyr::select(season, competition, comp_logo=file_path)

  df <- dplyr::left_join(df, comp_logos, by=c("season", "competition"))

  n_seasons = length(selected_seasons)

  reactable::reactable(
    data = df,
    pagination = FALSE,
    groupBy = if (inc_cup_games == "Yes") {
      c("season")
    },
    defaultSortOrder = "desc",
    striped = FALSE,
    defaultPageSize = length(df$season),
    compact = TRUE,
    style = list(
      fontSize = "smaller",
      fontWeight = 300,
      color = "black"
    ),
    defaultExpanded = ifelse(n_seasons==1 & length(selected_venue) > 1, TRUE, FALSE),
    rowStyle = if (inc_cup_games == "Yes") {
      reactable::JS("function(rowInfo) {
        if (rowInfo.aggregated === true) {
          if (rowInfo.isExpanded === true) {
            return {
              lineHeight: '2.5rem',
              fontWeight: 400,
              background: '#d7dee912'
            }
          } else {
            return {
              lineHeight: '2.5rem',
              fontWeight: 400
            }
          }
        }
      }")
    },
    defaultColDef = reactable::colDef(
      vAlign = "center",
      footerStyle = list(
        lineHeight = "2.5rem",
        fontWeight = 500,
        vAlign = "center",
        headerClass = "bar-sort-header"
      )
    ),
    showSortIcon = FALSE,
    columns = list(
      season = reactable::colDef(
        name = "Season",
        align = "left",
        minWidth = 85,
        grouped = if (inc_cup_games == "Yes") {
          reactable::JS("function(cellInfo) {
            return cellInfo.value
          }")
        }
      ),
      competition = reactable::colDef(
        name = "Competition",
        align = "left",
        minWidth = 140,
        aggregate = if (inc_cup_games == "Yes") {
          reactable::JS("function(values, rows) {
            let comps = 0

            rows.forEach(function(row) {
              comps += 1
            })
            return 'All comps (' + comps + ')'
          }")
        },
        cell = reactable::JS("function(cellInfo) {
          let img_src = cellInfo.row['comp_logo'];
          let competition = cellInfo.value;

          img = `<img src='${img_src}' style='width:28px;' alt='${competition}'>`;

          return `
          <div style='display: flex; flex-direction: row;'>
            <div style='align-self:center; width:40px; margin:2px 0;'>${img}</div>
            <div style='text-align:left; align-self:center; margin-left:5px;'>${competition}</div>
          </div>
          `
        }"),
        html = TRUE,
        footer = if (n_seasons > 1) {
          "Average"
        },
        footerStyle = list(
          padding = "0.6rem 0",
          fontWeight = 400,
          textAlign = "right"
        )
      ),
      P = reactable::colDef(
        name = "P",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 40,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons

            sprintf("%.0f", av_games)
          }
        }
      ),
      W = reactable::colDef(
        name = "W",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 35,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons

            sprintf("%.0f", av_games)
          }
        }
      ),
      D = reactable::colDef(
        name = "D",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 35,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons

            sprintf("%.0f", av_games)
          }
        }
      ),
      L = reactable::colDef(
        name = "L",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 35,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons

            sprintf("%.0f", av_games)
          }
        }
      ),
      GF = reactable::colDef(
        name = "GF",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 45,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons

            sprintf("%.0f", av_games)
          }
        }
      ),
      GA = reactable::colDef(
        name = "GA",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 45,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons

            sprintf("%.0f", av_games)
          }
        }
      ),
      GD = reactable::colDef(
        name = "GD",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        align = "center",
        minWidth = 50,
        footer = if (n_seasons > 1) {
          function(values) {
            games = sum(values)
            av_games = games / n_seasons
            sprintf("%.0f", av_games) # formatted av_games
          }
        }
      ),
      Pts = reactable::colDef(
        name = "Lge Pts",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        na = "-",
        align = "center",
        minWidth = 50,
        footer = if (n_seasons > 1) {
          reactable::JS("function(colInfo) {
            let total_pts = 0
            let ssns = 0
            colInfo.data.forEach(function(row) {
                total_pts += row[colInfo.column.id]
                ssns += 1
            })
            av_pts = total_pts / ssns
            return Number(av_pts.toFixed(0))
          }")
        }
      ),
      PPG = reactable::colDef(
        name = "Lge PPG",
        aggregate = if (inc_cup_games == "Yes") {
          "sum"
        },
        na = "-",
        align = "right",
        minWidth = 60,
        format = reactable::colFormat(
          digits = 2
        ),
        style = function(value) {
          if (!is.na(value)) {
            value
            normalized <- value / 3
            color <- green_pal(normalized)
            list(background = color)
          }
        },
        footer = if (n_seasons > 1) {
          reactable::JS("function(colInfo) {
            let total_ppg = 0
            let ssns = 0
            colInfo.data.forEach(function(row) {
                total_ppg += row[colInfo.column.id]
                ssns += 1
            })
            av_ppg = total_ppg / ssns
            return Number(av_ppg.toFixed(2))
          }")
        }
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        minWidth = 70,
        align = "right",
        aggregate = if (inc_cup_games == "Yes") {
          reactable::JS("function(values, rows) {
            let games_played = 0
            let wins = 0
            rows.forEach(function(row) {
              games_played += row['P']
              wins += row['W']
            })
            return wins / games_played
          }")
        },
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        ),
        style = function(value) {
          if (!is.na(value)) {
            value
            normalized <- (value - 0 / 1 - 0)
            color <- green_pal(normalized)
            list(background = color)
          }
        },
        footer = if (n_seasons > 1) {
          reactable::JS("function(colInfo) {
            const formatter = Intl.NumberFormat('en-US', {
              style: 'percent',
              maximumFractionDigits: 1,
            })

            let games_played = 0
            let wins = 0

            colInfo.data.forEach(function(row) {
              games_played += row['P']
              wins += row['W']
            })

            let win_pc = wins / games_played

            return formatter.format(win_pc)
          }")
        }
      ),
      comp_logo = reactable::colDef(show=FALSE)
    )
  )
}
