output_mgr_plr_records <- function(mgr_name) {

  df <- player_apps %>%
    dplyr::left_join(
      results_dataset,
      by = c(
        "season",
        "game_date",
        "game_no"
      )
    ) %>%
    dplyr::filter(
      manager == mgr_name
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      wins = sum(role == "starter" & outcome == "W"),
      win_pc = wins / starts,
      goals = sum(goals_scored),
      mins_played = sum(mins_played),
      subbed_off = sum(!is.na(off_for)),
      subbed_off_pc = subbed_off / starts,
      sub_apps = sum(role == "sub"),
      sub_apps_pc = sub_apps / (starts + sub_apps)
    ) %>%
    dplyr::arrange(
      dplyr::desc(subbed_off_pc)
    )

  plr_positions <- as.list(
    player_positions %>%
      dplyr::select(pl_index, position) %>%
      dplyr::pull(position) %>%
      purrr::set_names(player_positions %>% dplyr::pull(pl_index))
  )
  plr_headshots <- as.list(
    player_imgs %>%
      dplyr::select(pl_index, headshot_file_path) %>%
      dplyr::pull(headshot_file_path) %>%
      purrr::set_names(player_imgs %>% dplyr::pull(pl_index))
  )

  reactable::reactable(
    data = df,
    meta = list(
      positions = plr_positions,
      headshots = plr_headshots
    ),
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
    defaultSorted = list("starts" = "desc"),
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(df$menu_name)),
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        show = TRUE,
        width = 200,
        defaultSortOrder = "asc",
        cell = reactable::JS("function(cellInfo, state) {
          const player = cellInfo.value;
          const plr_name = player.split(' (b.')[0];
          const plr_pos = state.meta.positions[player];
          const img_src = state.meta.headshots[player];

          img = `<img src='${img_src}' style='height:45px; margin:auto;' alt='${plr_name}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:50px;'>
              ${img}
            </div>
            <div style='text-align:left; margin:auto 10px; line-height:1rem;'>
              ${plr_name}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${plr_pos}
              </span>
            </div>
          </div>`
        }"),
        html = TRUE
      ),
      starts = reactable::colDef(
        name = "Starts",
        show = TRUE
      ),

      wins = reactable::colDef(
        name = "Wins",
        show = TRUE
      ),
      win_pc = reactable::colDef(
        name = "Win Rate",
        show = TRUE,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      ),
      goals = reactable::colDef(
        name = "Goals",
        show = TRUE
      ),
      mins_played = reactable::colDef(
        name = "Mins Played",
        show = TRUE,
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      subbed_off = reactable::colDef(
        name = "Subbed Off",
        show = TRUE
      ),
      subbed_off_pc = reactable::colDef(
        name = "Subbed Off %",
        show = TRUE,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
        show = TRUE
      ),
      sub_apps_pc = reactable::colDef(
        name = "Sub Apps %",
        show = TRUE,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      )
    )
  )

}
