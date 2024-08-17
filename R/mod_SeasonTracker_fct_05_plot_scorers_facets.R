plot_top_scorers <- function(selected_seasons, inc_cup_games, n_scorers) {

  results <- filter_ssn_results(selected_seasons)

  if (inc_cup_games == "No") {
    results <- results %>%
      dplyr::filter(
        game_type == "League"
      )
  }

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
      n = n_scorers
    ) %>%
    dplyr::mutate(
      surname = stringr::str_split_i(player_name, " ", 2),
      display_name = stringr::str_replace(player_name, "\\s", "\n"),
      ssn_name = paste0(season, " ", display_name)
    )

  if ("League" %in% names(ssn_top)) {
    ssn_top <- ssn_top %>%
      dplyr::arrange(
        season,
        Total,
        League,
        desc(surname)
      )
  } else {
    ssn_top <- ssn_top %>%
      dplyr::arrange(
        season,
        Total,
        desc(surname)
      )
  }

  df <- goals_long %>%
    dplyr::inner_join(
      ssn_top,
      by = c("season", "player_name")
    ) %>%
    dplyr::mutate(
      surname = stringr::str_split_i(player_name, " ", 2),
      # display_name = stringr::str_replace(player_name, "\\s", "\n"),
      ssn_name = paste0(season, " ", display_name)
    )

  if ("League" %in% names(df)) {
    df <- df %>% dplyr::arrange(
      season,
      Total,
      League,
      player_name
    )
  } else {
    df <- df %>%
      dplyr::arrange(
      season,
      Total,
      player_name
    )
  }

  df$generic_comp <- factor(
    df$generic_comp,
    levels = c(
      "FA Trophy",
      "War League",
      "Anglo-Italian Cup",
      "Zenith Data Systems Cup",
      "Full Members' Cup",
      "Associate Members' Cup",
      "League Cup",
      "FA Cup",
      "League"
    )
  )


  p <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = factor(ssn_name, levels = unique(ssn_top$ssn_name)),
      y = n_goals,
      fill = generic_comp
    )
  ) +
    ggplot2::geom_bar(
      position = "stack",
      stat = "identity",
      alpha = 0.92
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
      linewidth = 0.2
    ) +
    ggtext::geom_textbox(
      ggplot2::aes(
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
    ggplot2::scale_x_discrete(
      labels = setNames(df$surname, df$ssn_name),
      expand = c(0.0, 0.3)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(
        mult = c(0.01, 0),
        add = c(0, 0.6)
      ),
      breaks = seq(
        from = 0,
        to = max(ssn_top$Total),
        by = ifelse(max(ssn_top$Total) < 20, 5, 10)
      ),
      limits = c(0, dplyr::case_when(
        length(selected_seasons) == 1 ~ max(ssn_top$Total) + 0.25,
        .default = max(ssn_top$Total) + 3
        )
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::facet_wrap(
      ~ season,
      ncol = 2,
      scales = "free_y",
      strip.position = "top"
    ) +
    ggplot2::theme_classic() +
    facet_wrap_theme() +
    ggplot2::scale_fill_manual(
      guide = ggplot2::guide_legend(title = NULL),
      values = c(
        "League" = "#414C6B",
        "FA Cup" = "steelblue",
        "League Cup" = "#B8CFEC",
        "Associate Members' Cup" = "#A5DEF2",
        "Full Members' Cup" = "grey70",
        "Zenith Data Systems Cup" = "grey70",
        "Anglo-Italian Cup" = "lightgreen",
        "War League" = "seashell",
        "FA Trophy" = "grey89"
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
    )

  if (n_scorers == 1) {
    height_px = 160
  } else {
    height_px = 350
  }

  shiny::renderPlot(
    p,
    height = ceiling(length(selected_seasons) / 2) * height_px,
    bg = "transparent"
  )

}
