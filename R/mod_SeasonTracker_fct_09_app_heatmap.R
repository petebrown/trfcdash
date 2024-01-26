heatmap_reactable <- function (selected_season, inc_cup_games = "Yes", pens_as_draw = "Yes", min_starts = 0, summary_stats, selected_stat = "mins_played", player_roles) {

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
      role %in% player_roles
    ) %>%
    dplyr::mutate(
      cards = dplyr::case_when(
        yellow_cards == 1 & red_cards == 0 ~ "Y",
        yellow_cards == 0 & red_cards == 1 ~ "R",
        yellow_cards == 1 & red_cards == 1 ~ "YR",
        TRUE ~ ""
      )
    ) %>%
    dplyr::left_join(
      game_nos,
      by = c(
        "game_date"
      )
    )

  pivot_df <- function(df, selected_stat) {
    df %>%
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
  }

  by_mins_played <- pivot_df(df, "mins_played")
  by_goals_scored <- pivot_df(df, "goals_scored")
  by_cards <- pivot_df(df, "cards")
  df <- pivot_df(df, selected_stat)

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

  by_mins_played <- by_mins_played %>%
    dplyr::filter(
      menu_name %in% df$menu_name
    )
  by_goals_scored <- by_goals_scored %>%
    dplyr::filter(
      menu_name %in% df$menu_name
    )
  by_cards <- by_cards %>%
    dplyr::filter(
      menu_name %in% df$menu_name
    )

  reactable::reactable(
    df,
    class = "apps-reactable",
    compact = TRUE,
    pagination = FALSE,
    defaultSorted = c("menu_name"),
    defaultSortOrder = "asc",
    showSortIcon = FALSE,
    columns = list(
      menu_name = reactable::colDef(
        header = "Player",
        defaultSortOrder = "asc",
        sticky = "left",
        align = "left",
        minWidth = 155,
        style = list(
          background = "white"
        )
      ),
      starts = reactable:: colDef(
        header = with_tooltip("A", "Starts"),
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
        header = with_tooltip("S", "Substitute Appearances"),
        show = ifelse(
          "apps" %in% summary_stats & "sub" %in% player_roles,
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
        header = with_tooltip("G", "Goals"),
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
        header = with_tooltip("Mins", "Minutes played"),
        show = ifelse(
          "mins_played" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 60,
        align = "right",
        format = reactable::colFormat(
          separators = TRUE
        ),
        style = list(
          background = "white"
        )
      ),
      yc = reactable:: colDef(
        header = with_tooltip("YC", "Yellow Cards"),
        show = ifelse(
          "cards" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 50,
        style = list(
          background = "yellow"
        )
      ),
      rc = reactable:: colDef(
        header = with_tooltip("RC", "Red Cards"),
        show = ifelse(
          "cards" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        minWidth = 50,
        style = list(
          background = "red"
        )
      ),
      win_pc = reactable:: colDef(
        header = with_tooltip("Win %", "Win %<br>(Starts only)"),
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
        header = with_tooltip("GPG", "Games-per-goal"),
        show = ifelse(
          "games_per_goal" %in% summary_stats,
          TRUE,
          FALSE
        ),
        sticky = "right",
        defaultSortOrder = "asc",
        minWidth = 56,
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
        header = with_tooltip("PPG", "Points-per-game (League)"),
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
      header = function(index, column, value) {
        game_df <- res %>% dplyr::filter(game_no == column)

        matchday_tooltip(game_df)
      },
      headerVAlign = "bottom",
      headerStyle = "margin: 1px;",
      minWidth = 33,
      align = "center",
      defaultSortOrder = "desc",
      sortNALast = TRUE,
      style = function(index, column, value) {
        mins_played <- by_mins_played[[column, value]]
        goals_scored <- by_goals_scored[[column, value]]
        cards <- by_cards[[column, value]]
        if (is.na(mins_played)) {
          # If player didn't play in game, set background to grey
          bg_color = "#f4f1f5"
        } else {
          normalized <- ifelse(
            mins_played > 90,  # logic for AET games
            90 / 90,
            mins_played / 90
          )
          bg_color <- green_pal(normalized)
        }
        list(
          background = bg_color,
          fontWeight = "400",
          fontSize = ifelse(selected_stat == "mins_played" & mins_played >= 100, "x-small", "small"),
          borderColor = if (is.na(cards) | cards == "") {
            "white"
          } else if (cards == "Y") {
            "gold"
          } else if (cards == "R") {
            "red"
          } else if (cards == "YR") {
            "gold red red gold"
          },
          borderWidth = "1.5px",
          borderStyle = "solid",
          color = ifelse(selected_stat == "goals_scored" & goals_scored == 0, bg_color, "black"),
          margin = "1px"
        )
      }
    )
  )
}
