heatmap_reactable <- function (selected_season, inc_cup_games = "Yes", pens_as_draw = "Yes", min_starts = 0) {

  res <- filter_ssn_results(selected_season) %>%
    filter_inc_cup_games(., inc_cup_games) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    )

  game_nos <- res %>%
    dplyr::select(
      game_date,
      game_no
    )

  df <- player_apps %>%
    dplyr::select(
      -game_no
    ) %>%
    dplyr::filter(
      game_date %in% res$game_date
    ) %>%
    dplyr::left_join(
      game_nos,
      by = c(
        "game_date"
      )
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

  stats <- player_apps %>%
    dplyr::inner_join(
      res,
      by = c(
        "season",
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
        minWidth = 120,
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
        style = list(
          color = "slategrey",
          background = "white"
        )
      ),
      mins_played = reactable:: colDef(
        name = "M",
        # sticky = "right",
        minWidth = 55,
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
        minWidth = 55,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 0
        ),
        style = function(value) {
          if (!is.na(value)) {
            list(background = "gold")
          }
        }
      )
    ),
    defaultColDef = reactable::colDef(
      minWidth = 32,
      align = "center",
      style = function(value) {
        if (is.na(value)) {
          color = "#f4f1f5"
        } else {
          value
          normalized <- ifelse(value > 90, 90 /90, value / 90) # logic for aet
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
