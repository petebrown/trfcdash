output_pl_summary_by_mgr <- function(inp_player_name) {
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
      manager
    ) %>%
    dplyr::summarise(
      apps = dplyr::n(),
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
      games_per_gl = round(mins_per_gl / 90, 2),
      mins_per_gl = round(mins_per_gl, 2),
      win_pc = round((W / starts) * 100, 1)
    ) %>%
    dplyr::arrange(
      dplyr::desc(apps)
    ) %>%
    dplyr::select(
      manager,
      apps,
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
    dplyr::select(
      where(
        ~sum(!is.na(.x)) > 0
      )
    )

  mgr_imgs <- manager_imgs %>%
    dplyr::select(
      manager_name,
      mgr_headshot = headshot_file_path
    )

  df <- dplyr::left_join(
    df,
    mgr_imgs,
    by = c("manager" = "manager_name")
  )

  reactable::reactable(
    df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    columns = list(
      manager = reactable::colDef(
        name = "",
        width = 200,
        vAlign = "top",
        cell = reactable::JS("function(cellInfo) {
          let img_src = cellInfo.row['mgr_headshot'];
          let manager = cellInfo.value;
          let borderRadius = 50;
          let border = '0.1pt black solid';

          if (manager === 'No manager') {
            borderRadius = 0;
            border = 'none';
          }

          img = `<img src='${img_src}' style='height: 50px; border-radius: ${borderRadius}%; border: ${border}' alt='${manager}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:60px;'>${img}</div>
            <div style='display:flex; margin-left:10px; text-align:left; align-items:center;'>${manager}</div>
          </div>
          `
        }"),
        html = TRUE
      ),
      apps = reactable::colDef(
        name = "Apps"
      ),
      starts = reactable::colDef(
        name = "Starts"
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps"
      ),
      win_pc = reactable::colDef(
        name = "Win %"
      ),
      mins_played = reactable::colDef(
        name = "Mins",
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      mins_per_gl = reactable::colDef(
        name = "Mins per goal",
        format = reactable::colFormat(
          digits = 0,
          separators = TRUE
        )
      ),
      games_per_gl = reactable::colDef(
        name = "Games per goal",
        format = reactable::colFormat(
          digits = 1
        )
      ),
      mgr_headshot = reactable::colDef(
        show = FALSE
      )
    )
  )
}
