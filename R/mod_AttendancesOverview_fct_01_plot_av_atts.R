plot_av_attendances <- function(year_range, cup_comps) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      venue == "H",
      ssn_year >= min_year,
      ssn_year <= max_year,
      generic_comp %in% c("Football League", "Non-League", cup_comps)
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarize(
      av_att = mean(attendance, na.rm = TRUE)
    )

  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = season,
      y = av_att
    )
  ) +
    ggplot2::geom_col(
      fill = "lightblue",
      color = "navy",
      linewidth = 0.2
    ) +
    ggplot2::geom_hline(
      yintercept = mean(df$av_att),
      linetype = "dashed",
      linewidth = 0.2
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete(
      breaks = df$season[c(T,F,F,F,F)]
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, vjust = 1)
    )


  return(plotly::ggplotly(p))

}
