get_h2h_summary <- function(df) {

  df %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(GF - GA),
      .groups = "drop"
    )
}

plot_h2h_summary <- function(df) {

  plot_data <- df %>%
    dplyr::group_by(outcome) %>%
    dplyr::summarise(
      n = dplyr::n(),
      pc = n / sum(n),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
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
      linewidth = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::coord_flip() +
    # remove legend
    ggplot2::guides(
      fill = "none"
    ) +
    # remove guidelines
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
      panel.grid.minor = ggplot2::element_blank()
    ) +
    # Remove y-axis labels
    ggplot2::scale_y_continuous(
      breaks = NULL,
      labels = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Wins" = "#A5DEF2",
        "Draws" = "steelblue",
        "Losses" = "#414C6B"
      )
    )  +
    ggtext::geom_textbox(
      ggplot2::aes(
        x = factor(outcome, levels = c("Losses", "Draws", "Wins")),
        y = n,
        label = n
      ),
      size = 5,
      halign = 0,
      hjust = 0,
      fill = NA,
      box.colour = NA,
      family = "Helvetica Neue"
    )
}
