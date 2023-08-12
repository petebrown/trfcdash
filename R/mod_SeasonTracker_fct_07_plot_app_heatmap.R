add_unused_levels <- function(data, var) {
  warning("This adds a row for each unused factor level to ensure plotly displays all levels in the legend. It should be used only for input within a ggplotly object.")

  var <- rlang::enquo(var)

  var_vctr <- dplyr::pull(data, !!var)

  unused_levels <- setdiff(levels(var_vctr), unique(var_vctr))

  if(length(unused_levels) != 0) data <- dplyr::bind_rows(data, tibble::tibble(!!var := unused_levels))

  return(data)
}

# plot_app_heatmap <- function(selected_season) {
selected_season == "1994/95"
ssn_games <- results_dataset %>%
  dplyr::filter(
    season == selected_season,
    game_type == "League"
  ) %>%
  dplyr::select(
    season,
    game_date,
    ssn_comp_game_no
  )

ssn_players <- player_apps %>%
  dplyr::filter(
    season == selected_season
  ) %>%
  dplyr::select(
    season,
    player_name
  ) %>%
  unique()

plot_data <- ssn_games %>%
  dplyr::left_join(
    ssn_players,
    by = "season",
    relationship = "many-to-many"
  ) %>%
  dplyr::left_join(
    player_apps,
    by = c(
      "season",
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::mutate(
    mins_played = tidyr::replace_na(mins_played, 0),
    goals_scored = tidyr::replace_na(goals_scored, 0),
    yellow_cards = tidyr::replace_na(yellow_cards, 0),
    red_cards = tidyr::replace_na(red_cards, 0),
    surname = stringr::str_split_i(player_name, " ", 2),
    forename = stringr::str_split_i(player_name, " ", 1),
    menu_name = stringr::str_glue("{surname}, {forename}"),
    menu_name = dplyr::case_match(
      menu_name,
      "Fon, Owain" ~ "Fon Williams, Owain",
      .default = menu_name
    )
  ) %>%
  dplyr::select(
    game_date,
    ssn_comp_game_no,
    player_name,
    menu_name,
    mins_played,
    goals_scored,
    role,
    yellow_cards,
    red_cards
  ) %>%
  dplyr::rename(
    game_no = ssn_comp_game_no
  ) %>%
  dplyr::left_join(
    results_dataset %>% dplyr::filter(game_type == "League") %>% dplyr::select(-game_no),
    by = "game_date"
  ) %>%
  dplyr::mutate(
    game_date = as.Date(game_date, format = '%Y-%m-%d'),
    game_date = format(game_date, "%d/%m/%Y"),
    menu_name = factor(
      menu_name,
      levels = rev(sort(unique(menu_name)))
    ),
    countfactor = cut(
      mins_played,
      breaks = c(seq(
        from = -10,
        to = 90,
        by = 10)
      ),
      labels = c(
        "0",
        "0-10",
        "10-20",
        "20-30",
        "30-40",
        "40-50",
        "50-60",
        "60-70",
        "70-80",
        "80-90"
      )
    ),
    countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor)))
  ) %>%
  add_unused_levels(countfactor)

p <-  ggplot2::ggplot(
  data = plot_data,
  ggplot2::aes(
    x = game_no,
    y = menu_name,
    fill = countfactor,
    text = sprintf("%s\n\n%s\n%s (%s)\nScore: %s\nMins played: %.0f\nGoals scored: %.0f\nYellow cards: %.0f\nRed cards: %.0f\n\n%s", player_name, opposition, game_date, venue, score, mins_played, goals_scored, yellow_cards, red_cards, season),
  )
) +
  ggplot2::geom_tile(
    color = ifelse(
      plot_data$red_cards == 1, "maroon", "grey50"
    ),
    linewidth = ifelse(
      plot_data$red_cards == 1, 1, 0.15
    )
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = ifelse(
        goals_scored > 0, goals_scored, ""
      )
    ),
    show.legend = FALSE,
    na.rm = TRUE,
    size = 3,
    color = ifelse(
      plot_data$mins_played < 50, "gray25", "gray85"
    )
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      title = "")
  ) +
  ggplot2::labs(
    title = paste0("Minutes played per game in ", selected_season),
    x = ggplot2::element_blank(),
    y = ggplot2::element_blank()
  ) +
  ggplot2::scale_y_discrete(
    expand = c(0, 0)
  ) +
  ggplot2::scale_x_discrete(
    expand = c(0, 0),
    breaks = c(seq(
      from = 1,
      to = 60,
      by = 5)
    )
  ) +
  ggplot2::scale_fill_manual(
    values = rev(c(
      "grey92",
      "#f7fbff",
      "#deebf7",
      "#c6dbef",
      "#9ecae1",
      "#6baed6",
      "#4292c6",
      "#2171b5",
      "#08519c",
      "#08306b"
    )),
    labels = c(
      "0",
      "0-10",
      "10-20",
      "20-30",
      "30-40",
      "40-50",
      "50-60",
      "60-70",
      "70-80",
      "80-90"
    ),
    drop = FALSE
  ) +
  ggplot2::theme_minimal(
    base_size = 12
  ) +
  ggplot2::theme(
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = ggplot2::element_text(colour = textcol),
    legend.margin = ggplot2::margin(grid::unit(0, "cm")),
    # legend.text = ggplot2::element_text(colour = textcol, size = 8, face="bold"),
    # legend.key.height = grid::unit(0.8, "cm"),
    # legend.key.width = grid::unit(0.2, "cm"),
    axis.text.x = ggplot2::element_text(size = 10, colour = textcol),
    axis.text.y = ggplot2::element_text(vjust = 0.2, colour = textcol),
    axis.ticks = ggplot2::element_line(size = 0.4),
    plot.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(0.7, 0.4, 0.1, 0.2, "cm"),
    plot.title = ggplot2::element_text(colour = textcol, hjust = 0, size = 14, face = "bold"),
    plot.title.position = "plot"
  ) +
  ggplot2::coord_cartesian(expand = FALSE)

plotly::ggplotly(p, tooltip = "text") %>%
  plotly::layout(
    legend = list(
      x = 0,
      xanchor = 'left',
      yanchor = 'bottom',
      orientation = 'h'),
    font = list(
      family = "Helvetica Neue"
    )
  )
# }
