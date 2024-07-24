get_h2h_by_venue <- function(df) {

  df %>%
    dplyr::group_by(
      venue
    ) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(GF - GA),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      venue = dplyr::case_match(
        venue,
        "H" ~ "Home",
        "A" ~ "Away",
        "N" ~ "Neutral"
      )
    ) %>%
    dplyr::arrange(
        factor(
          venue,
          levels = c("Home", "Away", "Neutral")
        )
      )
}

plot_h2h_by_venue <- function(df) {

  plot_data <- df %>%
    dplyr::group_by(
      venue,
      outcome
    ) %>%
    dplyr::summarise(
      n = dplyr::n(),
      pc = n / sum(n),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      venue = dplyr::case_when(
        venue == "H" ~ "Home",
        venue == "A" ~ "Away",
        TRUE ~ "Neutral"
      ),
      outcome = dplyr::recode(
        outcome,
        "W" = "Wins",
        "D" = "Draws",
        "L" = "Losses"
      )
    )

  ggplot2::ggplot(
    data = plot_data,
    mapping = ggplot2::aes(
      x = factor(
        outcome,
        levels = c(
          "Losses",
          "Draws",
          "Wins"
        )
      ),
      y = n,
      fill = outcome
    )
  ) +
    ggplot2::geom_col(
      color = "black",
      linewidth = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = "black",
      linewidth = 0.3
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::coord_flip() +
    ggplot2::guides(
      fill = "none"
    ) +
    ggplot2::facet_wrap(
      ~factor(
        venue,
        levels = c("Home", "Away", "Neutral")
      )
    ) +
    ggplot2::theme(
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
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        family = "Helvetica Neue",
        size = 14,
        face = "bold"
      ),
      axis.ticks.y = ggplot2::element_line(
        size = 0.5
      ),
    ) +
    ggplot2::scale_x_discrete(
      expand = c(0, 0.1)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      breaks = NULL,
      labels = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Wins" = "#A5DEF2",
        "Draws" = "steelblue",
        "Losses" = "#414C6B"
      )
    )
}
