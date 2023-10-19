get_h2h_summary <- function(opponent, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      opposition == opponent,
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
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
    )

  return(df)
}

plot_h2h_summary <- function(opponent, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  plot_data <- results_dataset %>%
    dplyr::filter(
      opposition == opponent,
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
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
      fill = FALSE
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
    )
}
