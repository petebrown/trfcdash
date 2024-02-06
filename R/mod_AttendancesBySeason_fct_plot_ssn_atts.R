plot_season_atts <- function(selected_seasons, cup_comps) {

  df <- results_dataset %>%
    dplyr::filter(
      venue == "H",
      season %in% selected_seasons,
      generic_comp %in% c("Football League", "Non-League", cup_comps)
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    )

  ssn_avs <- df %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarise(
      av_att = mean(attendance, na.rm = TRUE)
    )

  ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = game_no,
      y = attendance
    )
  ) +
    ggplot2::geom_col(
      fill = "lightblue",
      color = "black",
      linewidth = 0.3
    ) +
    ggplot2::geom_hline(
      data = ssn_avs,
      ggplot2::aes(
        yintercept = av_att
      ),
      color = "darkblue",
      linewidth = 0.25
    ) +
    ggplot2::labs(
      title = ggplot2::element_blank(),
      x = ggplot2::element_blank(),
      y = ggplot2::element_blank()
    ) +
    ggplot2::theme_minimal() +
    facet_wrap_theme() +
    ggplot2::facet_wrap(
      ~season,
      scales = "free_x"
    )

}
