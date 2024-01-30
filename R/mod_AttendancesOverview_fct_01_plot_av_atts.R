plot_av_attendances <- function(year_range) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      venue == "H",
      game_type == "League",
      ssn_year >= min_year,
      ssn_year <= max_year,
      !is.na(attendance)
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarize(
      av_att = mean(attendance)
    )

  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = season,
      y = av_att
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::geom_hline(
      yintercept = mean(df$av_att),
      linetype = "dashed",
      linewidth = 0.2
    ) +
    ggplot2::theme_minimal()

  return(plotly::ggplotly(p))

}
