heatmap_reactable <- function (selected_season, inc_cup_games, pens_as_draw, min_starts) {
  df <- player_apps %>%
    dplyr::filter(
      season == selected_season
    ) %>%
    dplyr::select(
      game_no,
      menu_name,
      mins_played
    ) %>%
    tidyr::pivot_wider(
      names_from = game_no,
      values_from = mins_played,
      values_fill = NA
    ) %>%
    dplyr::arrange(
      menu_name
    )

  res <- results_dataset %>%
    dplyr::filter(
      season == selected_season,
      dplyr::case_when(
        inc_cup_games == "No" ~ game_type == "League",
        TRUE ~ TRUE
      )
    )

  stats <- player_apps %>%
    dplyr::right_join(
      res,
      by = c(
        "season",
        "game_no",
        "game_date"
      )
    ) %>%
    dplyr::mutate(
      mutate_pens_as_draw(., pens_as_draw)
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%

    dplyr::summarise(
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      mins_played = sum(mins_played, na.rm = TRUE),
      goals = sum(goals_scored, na.rm = TRUE),
      yc = sum(yellow_cards, na.rm = TRUE),
      rc = sum(red_cards, na.rm = TRUE),
      win_pc = sum(outcome == "W" & role == "starter") / sum(role == "starter")
    )

  df <- df %>%
    dplyr::left_join(
      stats,
      by = "menu_name"
    ) %>%
    dplyr::filter(
      starts >= min_starts
    )

  reactable::reactable(
    df,
    compact = TRUE,
    pagination = FALSE,
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        sticky = "left",
        align = "left",
        minWidth = 150,
        style = list(
          background = "white"
        )
      ),
      starts = reactable:: colDef(
        name = "A",
        # sticky = "right",
        minWidth = 40,
        align = "right",
        style = list(
          background = "white"
        )
      ),
      sub_apps = reactable:: colDef(
        name = "S",
        # sticky = "right",
        minWidth = 40,
        align = "right",
        format = reactable::colFormat(
          prefix = " (",
          suffix = ")"
        ),
        style = list(
          background = "white"
        )
      ),
      mins_played = reactable:: colDef(
        name = "M",
        # sticky = "right",
        minWidth = 70,
        align = "right",
        format = reactable::colFormat(
          separators = TRUE
        ),
        style = list(
          background = "white"
        )
      ),
      goals = reactable:: colDef(
        name = "G",
        # sticky = "right",
        minWidth = 40,
        style = list(
          background = "white"
        )
      ),
      yc = reactable:: colDef(
        name = "YC",
        # sticky = "right",
        minWidth = 40,
        style = list(
          background = "yellow"
        )
      ),
      rc = reactable:: colDef(
        name = "RC",
        # sticky = "right",
        minWidth = 40,
        style = list(
          background = "tomato"
        )
      ),
      win_pc = reactable:: colDef(
        name = "Win %",
        # sticky = "right",
        minWidth = 50,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 0
        ),
        style = list(
          background = "gold"
        )
      )
    ),
    defaultColDef = reactable::colDef(
      minWidth = 30,
      align = "center",
      style = function(value) {
        if (is.na(value)) {
          color = "#f4f1f5"
        } else {
          value
          normalized <- (value / 90)
          color <- green_pal(normalized)
        }
        list(
          background = color,
          fontSize = "x-small",
          border = "solid white",
          borderWidth = "thin"
        )
      }
    )
  )
}
