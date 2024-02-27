add_unused_levels <- function(data, var) {
  var <- rlang::enquo(var)

  var_vctr <- dplyr::pull(data, !!var)

  unused_levels <- setdiff(levels(var_vctr), unique(var_vctr))

  if(length(unused_levels) != 0) data <- dplyr::bind_rows(data, tibble::tibble(!!var := unused_levels))

  return(data)
}

output_app_heatmap <- function(selected_season) {
  textcol = "grey40"

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

  if (selected_season == "2023/24") {
    s2324_missing <- dplyr::tibble(
      season = "2023/24",
      game_date = as.Date("2024-05-01"),
      ssn_comp_game_no = (max(ssn_games$ssn_comp_game_no) + 1):46
    )

    ssn_games <- dplyr::bind_rows(
      ssn_games,
      s2324_missing
    )
  }

  league_games <- results_dataset %>%
    dplyr::filter(
      season == selected_season,
      game_type == "League"
    )

  ssn_players <- player_apps %>%
    dplyr::filter(
      game_date %in% league_games$game_date
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
      mins_played = dplyr::case_when(
        mins_played == 0 ~ 1,
        is.na(mins_played) ~ -1,
        TRUE ~ mins_played
      ),
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
          "1-10",
          "11-20",
          "21-30",
          "31-40",
          "41-50",
          "51-60",
          "61-70",
          "71-80",
          "81-90"
        )
      ),
      countfactor = factor(as.character(countfactor), levels = rev(levels(countfactor)))
    )

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
      color = dplyr::case_when(
        plot_data$red_cards == 1 ~ "maroon",
        plot_data$yellow_cards == 1 ~ "gold1",
        .default = "grey50"
      ),
      linewidth = dplyr::case_when(
        plot_data$red_cards == 1 ~ 1.5,
        plot_data$yellow_cards == 1 ~ 0.6,
        .default = 0.15
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
      fontface = "bold",
      size = 4.5,
      color = ifelse(
        plot_data$mins_played < 40, "gray25", "gray85"
      )
    ) +
    ggplot2::labs(
      title = ggplot2::element_blank(),
      fill = ggplot2::element_blank(),
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_discrete(
      drop = FALSE,
      na.translate = FALSE,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_continuous(
      position = "top",
      breaks = seq(
        from = 2,
        to = ifelse(selected_season == "2023/24", 46, max(plot_data$game_no, na.rm = TRUE)),
        by = 2
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
      labels = rev(c(
        "0",
        "1-10",
        "11-20",
        "21-30",
        "31-40",
        "41-50",
        "51-60",
        "61-70",
        "71-80",
        "81-90"
      )),
      na.value = "grey92",
      drop = FALSE,
      guide = ggplot2::guide_legend(
        nrow = 1,
        reverse = TRUE
      )
    ) +
    ggplot2::theme_minimal(
      base_size = 12
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      # legend.title = ggplot2::element_blank(),
      # legend.margin = ggplot2::margin(grid::unit(0, "cm")),
      legend.text = ggplot2::element_text(colour = textcol, size = 12),
      # legend.key.height = grid::unit(0.8, "cm"),
      # legend.key.width = grid::unit(0.2, "cm"),
      axis.text.x = ggplot2::element_text(size = 12, colour = "grey60", face = "bold"),
      axis.text.y = ggplot2::element_text(size = 14, vjust = 0.2, colour = textcol),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_line(linewidth = 0.25),
      plot.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      # plot.margin = ggplot2::margin(0.7, 0.4, 0.1, 0.2, "cm"),
      plot.title = ggplot2::element_text(colour = textcol, hjust = 0, size = 14, face = "bold"),
      plot.title.position = "plot",
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
        rotate_margins = NA
      ),
      legend.key = ggplot2::element_rect(fill = "white", colour = "black")
    ) +
    ggplot2::coord_cartesian(expand = FALSE)
  p
}
