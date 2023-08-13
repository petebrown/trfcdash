get_max_goals <- function(selected_seasons) {

  goals_for <- player_apps %>%
    dplyr::filter(season %in% selected_seasons) %>%
    dplyr::filter(
      !is.na(goals_scored),
      player_name != "OG"
    ) %>%
    dplyr::group_by(
      season,
      player_name
    ) %>%
    dplyr::summarise(
      total_goals = sum(goals_scored)
    )

  max_goals <- max(goals_for$total_goals)

  return(max_goals)
}

plot_ssn_scorers <- function(selected_season, max_goals, n_plots) {
  results <- filter_ssn_results(selected_season)

  goals_for <- goals %>%
    dplyr::inner_join(
      results,
      by = c(
        "game_date"
      )
    ) %>%
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
      n_goals = dplyr::n(),
      .groups = "drop"
    )

  summaries <- summaries_long %>%
    dplyr::group_by(
      season,
      player_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      n_goals = sum(n_goals)
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
    ) %>%
    dplyr::mutate(
      surname = stringr::str_split_i(player_name, " ", 2),
      player_name = stringr::str_replace(player_name, "\\s", "\n")
    )

  df$generic_comp <- factor(df$generic_comp, levels = c("Anglo-Italian Cup", "Associate Members' Cup", "FA Cup", "Full Members' Cup", "League Cup", "League"))

  p <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = factor(ordered, levels = ssn_top$ordered),
      y = n_goals,
      fill = generic_comp,
      text = sprintf("Player: %s\nCompetition: %s\nGoals: %.0f", player_name, generic_comp, n_goals)
    ),
  ) +
    ggplot2::geom_bar(position = "stack", stat = "identity") +
    ggplot2::scale_x_discrete(
      labels = setNames(df$surname, df$ordered),
      expand = c(0, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0), add = c(0.1, dplyr::case_when(
        max_goals >= 20 & (n_plots %% 3 == 0) ~ (max_goals / 20) * 3,
        max_goals >= 20 & (n_plots %% 2 == 0) ~ (max_goals / 20) * 2.6,
        max_goals >= 20 & n_plots == 1 ~ max_goals / 20,
        max_goals < 20 ~ 0.5,
        TRUE ~ (max_goals / 20) * 3))
      ),
      breaks = seq(
        from = 0,
        to = max_goals,
        by = ifelse(max_goals < 20, 5, 10)
      ),
      limits = c(0, max_goals)
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      text = ggtext::element_markdown(
        family = "Helvetica Neue",
        face = "plain",
        size = 16,
        colour = NULL,
        fill = NA,
        box.colour = NA,
        linetype = NA,
        linewidth = NA,
        hjust = NULL,
        vjust = NULL,
        halign = "right",
        valign = NA,
        angle = NULL,
        lineheight = NULL,
        margin = NULL,
        padding = NA,
        r = NA,
        align_widths = NA,
        align_heights = NA,
        rotate_margins = NA,
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
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2:: element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "lightgrey",
                                                 size = 0.1),
      panel.background = ggplot2::element_rect(fill = "transparent"), #transparent panel bg
      plot.background = ggplot2::element_rect(fill = "transparent", color=NA), #transparent plot bg
      legend.background = ggplot2::element_rect(fill = "transparent"), #transparent legend bg
      legend.box.background = ggplot2::element_rect(fill = "transparent") #transparent legend panel
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Associate Members' Cup" = "#A5DEF2",
        "FA Cup" = "steelblue",
        "League" = "#414C6B",
        "League Cup" = "#B8CFEC"
      )
    ) +
    ggtext::geom_textbox(ggplot2::aes(
      x = factor(ordered, levels = ssn_top$ordered),
      y = Total,
      label = Total,
      fontface = "plain"
    ),
    size = dplyr::case_when(
      n_plots %% 2 == 0 ~ 4.75,
      n_plots == 1 ~ 5,
      TRUE ~ 4.5
    ),
    halign = 0,
    hjust = 0,
    fill = NA,
    box.colour = NA,
    family = "Helvetica Neue"
    )

  shiny::renderPlot(p, height = 150, bg = "transparent")

  # output_p <- plotly::ggplotly(p, tooltip="text") |> plotly::layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  #
  # plotly::renderPlotly({
  #   output_p
  # })
}
