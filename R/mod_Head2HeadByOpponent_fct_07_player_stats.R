get_h2h_player_stats <- function(res_df, show_images='Yes') {
  df <- res_df %>%
    dplyr::left_join(
      player_apps,
      by = c("game_date", "season", "game_no")
    ) %>%
    dplyr::group_by(
      menu_name
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
    dplyr::arrange(
      dplyr::desc(starts),
      dplyr::desc(mins_played),
      menu_name
    )

  if (show_images == 'Yes') {
    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        menu_name = stringr::str_glue("
        <div style='display: flex;'>
          <div style='display:flex; justify-content: center; width:50px;'>
            <img src='{map_plr_to_headshot(menu_name)}' style='height: 45px; margin: 2px;'>
          </div>
          <div style='padding-left: 10px; width: 100%; margin: auto;'>
            {menu_name}
          </div>
       </div>
      ")
      )
  }

  reactable::reactable(
    data = df,
    class = "apps_table_with_images",
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(df$menu_name)),
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      minWidth = 70,
      sortNALast = TRUE,
      vAlign = "center",
      headerVAlign = "center"
    ),
    compact = TRUE,
    style = list(
      fontSize = 'small',
      fontWeight = 300,
      color = "black"
    ),
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        sticky = "left",
        defaultSortOrder = "asc",
        minWidth = 185,
        grouped = if (show_images=='Yes') {
          reactable::JS("function(cellInfo) {
            const src = cellInfo.row['menu_name']
            return `
              <div>
                ${src}
              </div>
            `
          }")
        } else {
          reactable::JS("function(cellInfo) {
            return cellInfo.value
          }")
        },
        html = if (show_images=='Yes') {
          TRUE
        }
      ),
      apps = reactable::colDef(
        name = "Total Apps",
        align = "center"
      ),
      starts = reactable::colDef(
        name = "Starts",
        align = "center"
      ),
      sub_apps = reactable::colDef(
        name = "Sub Apps",
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
        align = "center"
      ),
      yellows = reactable::colDef(
        name = "Yellow Cards",
        align = "center"
      ),
      reds = reactable::colDef(
        name = "Red Cards",
        align = "center"
      ),
      mins_played = reactable::colDef(
        name = "Mins Played",
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
        cell = function(value) {
          if (!is.infinite(value) && !is.na(value)) {
            return (round(value, 2))
          } else {
            return ('-')
          }
        }
      ),
      winning_starts = reactable::colDef(
        show = FALSE
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        align = "right",
        na = "-",
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      )
    )
  )
}
