plot_top_scorers <- function(selected_seasons) {

  results <- filter_ssn_results(selected_seasons)

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

  goals_grouped <- goals_for %>%
    dplyr::group_by(
      season,
      player_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      n_goals = dplyr::n(),
      .groups = "drop"
    )

  goals_long <- goals_for %>%
    dplyr::group_by(
      season,
      player_name,
      generic_comp
    ) %>%
    dplyr::summarise(
      n_goals = dplyr::n(),
      .groups = "drop"
    )

  goals_wide <- goals_long %>%
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

  ssn_top <- goals_wide %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(
      order_by = Total,
      by = season,
      n = 3
    ) %>%
    dplyr::mutate(
      surname = stringr::str_split_i(player_name, " ", 2),
      display_name = stringr::str_replace(player_name, "\\s", "\n"),
      ssn_name = paste0(season, " ", display_name)
    ) %>%
    dplyr::arrange(
      season,
      Total,
      League,
      desc(surname)
    )

  df <- goals_long %>%
    dplyr::inner_join(
      ssn_top,
      by = c("season", "player_name")
    ) %>%
    dplyr::arrange(
      season,
      Total,
      League,
      player_name
    ) %>%
    dplyr::mutate(
      surname = stringr::str_split_i(player_name, " ", 2),
      # display_name = stringr::str_replace(player_name, "\\s", "\n"),
      ssn_name = paste0(season, " ", display_name)
    )

  View(df)

  df$generic_comp <- factor(df$generic_comp, levels = c("Anglo-Italian Cup", "Associate Members' Cup", "FA Cup", "Full Members' Cup", "League Cup", "League"))


  p <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = factor(ssn_name, levels = unique(ssn_top$ssn_name)),
      y = n_goals,
      fill = generic_comp,
      label = n_goals
    )
  ) +
    ggplot2::geom_bar(
      position = "stack",
      stat = "identity"
    ) +
    ggplot2::geom_col(
      data = ssn_top,
      mapping = ggplot2::aes(
        x = factor(ssn_name, levels = unique(ssn_name)),
        y = Total,
        fill = NULL,
        label = NULL
      ),
      color = "black",
      fill = NA,
      legend = FALSE,
      linewidth = 0.2
    ) +
    ggplot2::scale_x_discrete(
      labels = setNames(df$surname, df$ssn_name),
      expand = c(0, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::scale_y_continuous(
      # expand = ggplot2::expansion(
      #   mult = c(0, 0),
      #   add = c(0.1, 0.1)
      # ),
      breaks = seq(
        from = 0,
        to = max(ssn_top$Total),
        by = ifelse(max(ssn_top$Total) < 20, 5, 10)
      ),
      limits = c(0, max(ssn_top$Total))
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "bottom",
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
      panel.spacing = ggplot2::unit(2, "lines"),
      line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2:: element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "lightgrey",
                                                 size = 0.1),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.background = ggplot2::element_rect(
        fill = "transparent"
      ),
      legend.box.background = ggplot2::element_rect(
        fill = "transparent",
        color = "transparent",
        linewidth = 0
      )
    ) +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(title = NULL),
      values = c(
        "League" = "#414C6B",
        "FA Cup" = "steelblue",
        "League Cup" = "#B8CFEC",
        "Associate Members' Cup" = "#A5DEF2",
        "Full Members' Cup" = "lightgreen",
        "Zenith Data Systems Cup" = "lightgreen",
        "Anglo-Italian Cup" = "tomato",
        "War League" = "yellow",
        "FA Trophy" = "lightgrey"
      ),
      breaks = c(
        "League",
        "FA Cup",
        "League Cup",
        "Associate Members' Cup",
        "Full Members' Cup",
        "Zenith Data Systems Cup",
        "Anglo-Italian Cup",
        "War League",
        "FA Trophy"
      )
    ) +
    ggtext::geom_textbox(ggplot2::aes(
      x = factor(ssn_name, levels = unique(ssn_top$ssn_name)),
      y = Total,
      label = Total,
      fontface = "plain"
    ),
    size = 4.5,
    halign = 0,
    hjust = 0,
    fill = NA,
    box.colour = NA,
    family = "Helvetica Neue"
    ) +
    ggplot2::facet_wrap(
      ~ season,
      scales = "free_y",
      strip.position = "top"
    )

  p

}
