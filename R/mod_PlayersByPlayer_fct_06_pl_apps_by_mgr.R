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
      win_pc = round((W / starts) * 100, 1),
      mgr_name = manager
    ) %>%
    dplyr::arrange(
      dplyr::desc(apps)
    ) %>%
    dplyr::select(
      manager,
      mgr_name,
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
        width = 75,
        vAlign = "top",
        cell = function(value) {
          image <- img(
            src = dplyr::case_when(
              .default = paste0(
                "./www/images/managers/", tolower(gsub(' ', '-', value)), ".jpg"),
              value == "No manager" ~ "./www/images/crest.svg",
              stringr::str_detect(value, "Sheedy") ~ "./www/images/managers/kevin-sheedy.jpg",
              stringr::str_detect(value, "McAteer") ~ "./www/images/managers/jason-mcateer.jpg"
            ),
            style = dplyr::case_when(
              .default = "height: 50px; border-radius: 50%;",
              value == "No manager" ~ "height: 50px;"
            ),
            alt = value
          )
          tagList(
            div(style = "display: inline-block; width: 60px;", image)
          )
        }
      ),
      mgr_name = reactable::colDef(
        name = "",
        width = 130
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
      )
    )
  )
}
