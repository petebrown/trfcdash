plot_ssn_scorers <- function(selected_season) {
  results <- filter_ssn_results(selected_season)

  players <- get_player_apps_df() %>%
    dplyr::rename(
      game_id = sb_game_id,
      player_id = sb_player_id
    ) %>%
    dplyr::mutate(
      game_id = stringr::str_replace(game_id, "tpg", ""),
      game_id = as.numeric(game_id)
    )

  game_dates <- players %>%
    dplyr::select(game_id, game_date) %>%
    unique()

  goals_for <- get_goals_df() %>%
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

  summaries_long <- goals_for %>%
    dplyr::group_by(
      season,
      player_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      n_goals = dplyr::n()
    )

  summaries <- goals_for %>%
    dplyr::group_by(
      season,
      player_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      n_goals = dplyr::n()
    ) %>%
    tidyr::pivot_wider(
      id_cols = c(season, player_name),
      names_from = generic_comp,
      values_from = n_goals,
      values_fill = 0
    ) %>%
    dplyr::mutate(
      Total = rowSums(dplyr::across(where(is.numeric), sum))
    )

  ssn_top <- summaries %>%
    dplyr::group_by(season) %>%
    dplyr::slice_max(order_by = Total, n = 3) %>%
    dplyr::mutate(
      ordered = paste0(season, Total, League, player_name) %>% forcats::fct_inorder()
    ) %>%
    dplyr::arrange(
      season,
      Total,
      League,
      desc(player_name)
    )

  df <- summaries_long %>%
    dplyr::inner_join(ssn_top, by = c("season", "player_name")) %>%
    dplyr::mutate(
      ordered = paste0(season, Total, League, player_name) %>% forcats::fct_inorder()
    ) %>%
    dplyr::arrange(
      season,
      Total,
      League,
      player_name
    )

  df$generic_comp <- factor(df$generic_comp, levels = c("Associate Members' Cup", "FA Cup", "League Cup", "League"))

  p <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = factor(ordered, levels = ssn_top$ordered),
      y = n_goals,
      fill = generic_comp)
  ) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::scale_x_discrete(labels = setNames(df$player_name, df$ordered)) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0), add = c(0.15, 0))
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(
        family = "Helvetica Neue"
      ),
      strip.text.x = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      strip.background = ggplot2::element_rect(
        fill = "white",
        color="white"
      ),
      panel.border = ggplot2::element_blank(),
      line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Associate Members' Cup" = "#A5DEF2",
        "FA Cup" = "steelblue",
        "League" = "#414C6B",
        "League Cup" = "#B8CFEC"
      )
    )

  output_p <- plotly::ggplotly(p)

  plotly::renderPlotly({
    output_p
  })
}
