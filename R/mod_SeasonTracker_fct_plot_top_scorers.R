plot_ssn_scorers <- function(selected_season) {
  if (selected_season < 1996) {
    p("Goalscorer data only currently available from 1996/97")
  } else {
    results <- imported_results %>%
      dplyr::filter(season == selected_season)

    game_dates <- imported_players_df %>%
      dplyr::select(game_id, game_date) %>%
      unique()

    goals_for <- imported_goals_df %>%
      dplyr::filter(
        goal_type == "for",
        own_goal == 0
      ) %>%
      dplyr::left_join(game_dates, by = "game_id") %>%
      dplyr::inner_join(results, by = "game_date") %>%
      dplyr::mutate(
        generic_comp = dplyr::case_when(
          .default = generic_comp,
          generic_comp %in% c("Football League", "Non-League") ~ "League"
        )
      ) %>%
      dplyr::arrange(desc(game_date))

    ssn_scorers_long <- goals_for %>%
      dplyr::group_by(
        season,
        player_name,
        generic_comp
      ) %>%
      dplyr::summarise(
        n_goals = dplyr::n()
      ) %>% dplyr::arrange(season)

    ssn_scorers_wide <- ssn_scorers_long %>%
      tidyr::pivot_wider(
        id_cols = c(season, player_name),
        names_from = generic_comp,
        values_from = n_goals,
        values_fill = 0
      ) %>%
      dplyr::mutate(
        Total = rowSums(dplyr::across(where(is.numeric), sum))
      )

    ssn_top_3 <- ssn_scorers_wide %>%
      dplyr::group_by(season) %>%
      dplyr::slice_max(order_by = Total, n = 3) %>%
      dplyr::mutate(
        ordered = paste0(season, Total, League, player_name) %>% forcats::fct_inorder()
      ) %>%
      dplyr::arrange(season, Total, League, desc(player_name))

    df <- ssn_scorers_long %>%
      dplyr::inner_join(ssn_top_3, by = c("season", "player_name")) %>%
      dplyr::mutate(
        ordered = paste0(season, Total, League, player_name) %>% forcats::fct_inorder()
      ) %>%
      dplyr::arrange(season, Total, League, desc(player_name))

    p <- ggplot2::ggplot(
      data = df,
      ggplot2::aes(
        x = factor(ordered, levels = ssn_top_3$ordered),
        y = n_goals,
        fill = generic_comp,
        text = sprintf("Player: %s\nCompetition: %s\nGoals: %.0f", player_name, generic_comp, n_goals)
        )
      ) +
      ggplot2::geom_bar(
        position = "stack",
        stat = "identity"
      ) +
      # ggplot2::geom_text(ggplot2::aes(
      #   x = factor(ordered, levels = ssn_top_3$ordered),
      #   y = Total,
      #   label = Total
      #   ),
      #   color = "slategray",
      #   hjust = "right",
      #   nudge_y = 0.2
      # ) +
      ggplot2::scale_x_discrete(labels = setNames(df$player_name, df$ordered)) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = NULL,
        y = NULL
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0), add = c(0.1, 1))
      ) +
      ggplot2::theme_classic() +
      ggplot2::theme(
        legend.position = "none",
        text = ggplot2::element_text(
          family = "Helvetica Neue"
        ),
        line = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 10)
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "Associate Members' Cup" = "#A5DEF2",
          "FA Cup" = "steelblue",
          "League" = "#414C6B",
          "League Cup" = "#B8CFEC"
        )
      )

    output_p <- plotly::ggplotly(p, tooltip="text")

    plotly::renderPlotly({
      output_p
    })
  }
}
