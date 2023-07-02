#' 05_plot_scorers_circles
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plot_ssn_scorers_circles <- function(selected_season, max_goals, n_plots) {
  results <- filter_ssn_results(selected_season)

  players <- get_player_apps_df()

  goals_for <- get_goals_df() %>%
    dplyr::filter(
      player_name != "OG"
    ) %>%
    dplyr::inner_join(
      results,
      by = c("season" = "season", "game_no" = "ssn_game_no")
    ) %>%
    dplyr::mutate(
      generic_comp = dplyr::case_when(
        .default = generic_comp,
        generic_comp %in% c("Football League", "Non-League") ~ "League"
      )
    ) %>%
    dplyr::arrange(
      desc(game_date)
    )

  df <- goals_for %>%
    dplyr::mutate(
      generic_comp = factor(
        generic_comp,
        levels = rev(c("Anglo-Italian Cup", "Associate Members' Cup", "Full Members' Cup", "League Cup", "FA Cup", "League")))
    ) %>%
    dplyr::group_by(
      player_name,
      game_date
    ) %>%
    dplyr::slice(rep(1:dplyr::n(), each = goals_scored)) %>%
    dplyr::ungroup() %>%
    dplyr::add_count(
      player_name,
      name = "ssn_gls"
    ) %>%
    dplyr::add_count(
      player_name, generic_comp,
      name = "ssn_comp_gls"
    ) %>%
    dplyr::filter(
      dplyr::dense_rank(dplyr::desc(ssn_gls)) %in% 1:3
    ) %>%
    dplyr::group_by(player_name) %>%
    dplyr::arrange(
      player_name,
      generic_comp,
      game_date
    ) %>%
    dplyr::mutate(
      ssn_goal_no = dplyr::row_number()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      game_date,
      game_no,
      opposition,
      score,
      generic_comp,
      player_name,
      ssn_goal_no,
      goals_scored,
      ssn_comp_gls,
      ssn_gls
    )

  pl_order <- df %>%
    dplyr::select(player_name, ssn_gls) %>%
    unique() %>%
    dplyr::arrange(ssn_gls)

  pl_levels = pl_order$player_name

  df <- df %>%
    dplyr::arrange(
      factor(player_name, levels = pl_levels),
      ssn_goal_no
    )


  p <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = factor(player_name, levels = pl_levels),
      y = ssn_goal_no - 0.5,
      fill = generic_comp,
      text = sprintf("Player: %s\nCompetition: %s\nGoals: %.0f", player_name, generic_comp, ssn_gls)
    ),
  ) +
    ggplot2::geom_dotplot(
      binaxis = "y",
      binwidth = 1,
      stackdir = "center"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks=seq(0, max_goals + 1, 5)) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    # ggplot2::scale_y_continuous(
    #   expand = ggplot2::expansion(mult = c(0, 0), add = c(0.1, dplyr::case_when(
    #     max_goals >= 20 & (n_plots %% 3 == 0) ~ (max_goals / 20) * 3,
    #     max_goals >= 20 & (n_plots %% 2 == 0) ~ (max_goals / 20) * 2.6,
    #     max_goals >= 20 ~ max_goals / 20,
    #     max_goals < 20 ~ 0.5,
    #     TRUE ~ 0))
    #   ),
    #   breaks = seq(0, max_goals + 1, ifelse(max_goals < 20, 5, 10)),
    #   limits = c(0,  max_goals)
    # ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
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
      strip.text.x = ggplot2::element_text(
        hjust = 0.5,
        face = "bold"
      ),
      strip.background = ggplot2::element_rect(
        fill = "white",
        color="white"
      ),
      panel.border = ggplot2::element_blank(),
      line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2:: element_blank(),
      panel.grid.major.x = ggplot2::element_line(color = "lightgrey",
                                                 size = 0.1),
      panel.background = ggplot2::element_rect(fill = "transparent"), #transparent panel bg
      plot.background = ggplot2::element_rect(fill = "transparent", color=NA), #transparent plot bg
      legend.background = ggplot2::element_rect(fill = "transparent"), #transparent legend bg
      legend.box.background = ggplot2::element_rect(fill = "transparent") #transparent legend panel
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Associate Members' Cup" = "#A5DEF2",
        "FA Cup" = "steelblue",
        "League" = "#414C6B",
        "League Cup" = "#B8CFEC"
      )
    )
  # +
  #   ggtext::geom_textbox(ggplot2::aes(
  #     x = player_name,
  #     y = ssn_gls,
  #     label = ssn_gls
  #   ),
  #   size = dplyr::case_when(
  #     n_plots %% 3 == 0 ~ 4.5,
  #     n_plots %% 2 == 0 ~ 4.75,
  #     TRUE ~ 5
  #   ),
  #   halign = 0,
  #   hjust = 0,
  #   fill = NA,
  #   box.colour = NA,
  #   family = "Helvetica Neue"
  #   )

  shiny::renderPlot(p, height = 200, bg = "transparent")

  # output_p <- plotly::ggplotly(p, tooltip="text") |> plotly::layout(plot_bgcolor = "rgba(0,0,0,0)", paper_bgcolor = "rgba(0,0,0,0)")
  #
  # plotly::renderPlotly({
  #   output_p
  # })
}
