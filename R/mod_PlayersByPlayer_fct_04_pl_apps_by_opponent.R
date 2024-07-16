output_pl_summary_by_opp <- function(inp_player_name) {
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
      by = "game_date"
    ) %>%
    dplyr::group_by(
      opposition
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      mins_played = sum(mins_played),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      Goals = sum(goals_scored)
    ) %>%
    dplyr::mutate(
      mins_per_gl = dplyr::case_when(
        mins_played / Goals != Inf ~ mins_played / Goals,
        TRUE ~ NA
      ),
      games_per_gl = mins_per_gl / 90,
      mins_per_gl = mins_per_gl,
      win_pc = W / P
    ) %>%
    dplyr::arrange(
      dplyr::desc(P)
    ) %>%
    dplyr::select(
      opposition,
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
      games_per_gl
    ) %>%
    dplyr::rename(
      Opposition = opposition,
      "Mins\nplayed" = mins_played
    )

  reactable::reactable(
    df,
    meta = list(
      crests = meta_data[['club_crests']]
    ),
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      align = "right",
      vAlign = "center",
      minWidth = 50,
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    columns = list(
      Opposition = reactable::colDef(
        name = "Opposition",
        align = "left",
        minWidth = 150,
        cell = reactable::JS("function(cellInfo, state) {
          const { crests } = state.meta;

          let opponent = cellInfo.value;
          let img_src = crests[opponent];

          img = `<img src='${img_src}' style='height:32px; margin:2px;' alt='${opponent}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:40px;'>${img}</div>
            <div style='display:flex; text-align:left; margin:6.4px;'>${opponent}</div>
          </div>
          `
        }"),
        html = TRUE
      ),
      P = reactable::colDef(
        name = "Total Apps"
      ),
      starts = reactable::colDef(
        name = "Starts"
      ),
      sub_apps = reactable::colDef(
        name = "Sub apps",
        format = reactable::colFormat(
          prefix = '+'
        )
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      ),
      mins_per_gl = reactable::colDef(
        name = "Mins per goal",
        cell = function(value) {
          if (is.na(value)) {
            return ('-')
          } else {
            return (round(value, 1))
          }
        }
      ),
      games_per_gl = reactable::colDef(
        name = "Games per goal",
        cell = function(value) {
          if (is.na(value)) {
            return ('-')
          } else {
            return (round(value, 1))
          }
        }
      )
    )
  )
}
