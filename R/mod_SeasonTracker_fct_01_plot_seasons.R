output_seasons_plot <- function(selected_seasons, chosen_plot) {

  df <- filter_ssn_results(selected_seasons) %>%
    dplyr::filter(
      game_type == "League"
    ) %>%
    dplyr::select(
      season,
      ssn_comp_game_no,
      game_date,
      opposition,
      venue,
      outcome,
      score,
      scorers,
      goals_for,
      goals_against,
      competition,
      league_tier,
      manager,
      league_pos,
      pts,
      attendance
    ) %>%
    dplyr::rename(
      game_no = ssn_comp_game_no
    ) %>%
    dplyr::mutate(
      attendance = format(attendance, nsmall = 0, big.mark = ","),
      game_date = as.Date(game_date, format = '%d/%m/%Y'),
      game_date = format(game_date, "%e %B %Y"),
      ppg = pts / game_no
    )

  # Set lowest league position for League Position chart
  if (max(df$season) %in% selected_seasons) {
    max_pos = 24 # Current season
  } else if ("2019/20" %in% selected_seasons) {
    max_pos = 23 # Season ended early and Bury didn't compete
  } else {
      max_pos = (max(df$game_no) / 2) + 1
  }

  # Set Y-axis based on chart type
  if (isTRUE(selected_seasons == "2023/24" & max(df$game_no < 10))) {
    x_scale_var = ggplot2::scale_x_continuous(
      limits = c(1, 46)
    )
  } else {
    x_scale_var = ggplot2::scale_x_continuous(
      limits = c(1, max(df$game_no))
    )
  }

  # Set Y-axis based on chart type
  if (chosen_plot == "league_pos") {
    y_scale_var = ggplot2::scale_y_reverse(
      limits = c(max_pos, 1),
      breaks = c(1, 3, 7, 21, max_pos)
    )
  } else if (chosen_plot == "ppg") {
    y_scale_var = ggplot2::scale_y_continuous(
      limits = c(0, 3),
      breaks = c(0, 1, 2, 3)
    )
  } else if (isTRUE(chosen_plot == "pts" & selected_seasons == "2023/24")) {
    y_scale_var = ggplot2::scale_y_continuous(
      limits = c(0, max(df$game_no) * 3),
      breaks = c(0, max(df$game_no) * 3)
    )
  } else {
    y_scale_var = ggplot2::scale_y_continuous()
  }

  plot_types <- get_chart_options()

  df <- df %>%
    dplyr::mutate(
      tooltip_text = dplyr::case_when(
        goals_for == 1 ~ sprintf("Season: %s\nGame No: %.0f\nDate: %s\nOpposition: %s\nVenue: %s\nScore: %s\nScorer: %s\nLeague Tier: %.0f (%s)\nLeague Pos: %.0f\nTotal Points: %.0f\nPPG: %.2f\nManager: %s\nAttendance: %s", season, game_no, game_date, opposition, venue, score, scorers, league_tier, competition, league_pos, pts, ppg, manager, attendance),
        goals_for > 1 ~ sprintf("Season: %s\nGame No: %.0f\nDate: %s\nOpposition: %s\nVenue: %s\nScore: %s\nScorers: %s\nLeague Tier: %.0f (%s)\nLeague Pos: %.0f\nTotal Points: %.0f\nPPG: %.2f\nManager: %s\nAttendance: %s", season, game_no, game_date, opposition, venue, score, scorers, league_tier, competition, league_pos, pts, ppg, manager, attendance),
        goals_for == 0 ~ sprintf("Season: %s\nGame No: %.0f\nDate: %s\nOpposition: %s\nVenue: %s\nScore: %s\nLeague Tier: %.0f (%s)\nLeague Pos: %.0f\nTotal Points: %.0f\nPPG: %.2f\nManager: %s\nAttendance: %s", season, game_no, game_date, opposition, venue, score, league_tier, competition, league_pos, pts, ppg, manager, attendance)
      )
    )

  # Create the plot
  p = ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = game_no,
      y = get(chosen_plot),
      group = 1,
      text = tooltip_text)
    ) +
    ggplot2::geom_line(ggplot2::aes(color = season)) +
    ggplot2::geom_point(ggplot2::aes(color = season)) +
    ggplot2::labs(
      x = "Game no.",
      y = names(plot_types)[plot_types == chosen_plot]
    ) +
    x_scale_var +
    y_scale_var +
    ggplot2::scale_color_brewer(name = "", palette = "Paired") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 8),
      axis.text = ggplot2::element_text(size = 7)
    )

  output_p <- plotly::ggplotly(p, tooltip = "text")  |>
    plotly::layout(
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      legend = list(bgcolor = "rgba(0,0,0,0)")
    )

  return (output_p)
}
