heatmap_reactable <- function (selected_season, inc_cup_games = "Yes", pens_as_draw = "Yes", min_starts = 0, summary_stats, selected_stat = "mins_played", starters_only) {

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
      game_date %in% res$game_date,
      dplyr::case_when(
        starters_only == "Yes" ~ role == "starter",
        TRUE ~ TRUE
      )
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
      (!!selected_stat)
    ) %>%
    tidyr::pivot_wider(
      names_from = game_no,
      values_from = (!!selected_stat),
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
      goals = sum(goals_scored, na.rm = TRUE),
      mins_played = sum(mins_played, na.rm = TRUE),
      yc = sum(yellow_cards, na.rm = TRUE),
      rc = sum(red_cards, na.rm = TRUE),
      win_pc = sum(outcome == "W" & role == "starter") / sum(role == "starter"),
      games_per_goal = (mins_played / 90) / goals,
      ppg = ((sum(role == "starter" & outcome == "W" & game_type == "League") * 3) + sum(role == "starter" & outcome == "D" & game_type == "League")) / sum(role == "starter" & game_type == "League")
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
    class = "apps-reactable",
    compact = TRUE,
    pagination = FALSE,
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        sticky = "left",
        align = "left",
        minWidth = 155,
        style = list(
          background = "white"
        )
      ),
      starts = reactable:: colDef(
        name = "A",
        show = ifelse(
          "apps" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 40,
        align = "right",
        style = list(
          background = "white"
        )
      ),
      sub_apps = reactable:: colDef(
        name = "S",
        show = ifelse(
          "apps" %in% summary_stats & starters_only != "Yes",
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 40,
        align = "right",
        style = list(
          color = "slategrey",
          background = "white"
        )
      ),
      goals = reactable:: colDef(
        name = "G",
        show = ifelse(
          "goals" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 40,
        align = "right",
        style = list(
          background = "white"
        )
      ),
      mins_played = reactable:: colDef(
        name = "M",
        show = ifelse(
          "mins_played" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 55,
        align = "right",
        format = reactable::colFormat(
          separators = TRUE
        ),
        style = list(
          background = "white"
        )
      ),
      yc = reactable:: colDef(
        name = "YC",
        show = ifelse(
          "cards" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 40,
        style = list(
          background = "yellow"
        )
      ),
      rc = reactable:: colDef(
        name = "RC",
        show = ifelse(
          "cards" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 40,
        style = list(
          background = "tomato"
        )
      ),
      win_pc = reactable:: colDef(
        name = "Win %",
        show = ifelse(
          "win_pc" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 65,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 0
        ),
        style = function(value) {
          if (!is.na(value)) {
            list(background = "gold")
          }
        }
      ),
      games_per_goal = reactable:: colDef(
        name = "GPG",
        show = ifelse(
          "games_per_goal" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        defaultSortOrder = "asc",
        minWidth = 55,
        format = reactable::colFormat(
          digits = 1
        ),
        style = function(value) {
          if (!is.na(value) & !is.infinite(value)) {
            list(background = "lightblue")
          }
        }
      ),
      ppg = reactable:: colDef(
        name = "PPG",
        show = ifelse(
          "ppg" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 56,
        format = reactable::colFormat(
          digits = 2
        ),
        style = function(value) {
          if (!is.na(value)) {
            list(background = "lightblue")
          }
        }
      )
    ),
    defaultColDef = reactable::colDef(
      minWidth = 32,
      align = "center",
      defaultSortOrder = "desc",
      style = function(value, index) {
        if (is.na(value)) {
          # If player didn't play in game, set background to grey
          bg_color = "#f4f1f5"
        } else if (selected_stat == "mins_played") {
          normalized <- ifelse(value > 90, 90 / 90, value / 90) # logic for AET games
          bg_color <- green_pal(normalized)
        } else if (selected_stat == "goals_scored") {
          mins_stat <- df$mins_played[index]
          normalized <- ifelse(mins_stat > 90, 90 / 90, mins_stat / 90) # logic for AET games
          bg_color <- green_pal(normalized)
        }
        list(
          background = bg_color,
          fontSize = "x-small",
          border = "solid white",
          borderWidth = "thin",
          color = ifelse(selected_stat == "goals_scored" & value == 0, bg_color, "black")
        )
      }
    )
  )
}
