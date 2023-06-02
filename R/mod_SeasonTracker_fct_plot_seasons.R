# import_results_mini <- function() {
#   df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv", show_col_types = FALSE)
#   return (df)
# }
#
# import_results <- function() {
#   df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv", show_col_types = FALSE)
#   return (df)
# }
#
# import_goals_df <- function() {
#   df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/scrape-goals/main/data/goals.csv", show_col_types = FALSE)
#   return (df)
# }
#
# import_players_df <- function() {
#   df <- vroom::vroom("https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv", show_col_types = FALSE) %>%
#     dplyr::rename(
#       game_id = sb_game_id,
#       player_id = sb_player_id
#     ) %>%
#     dplyr::mutate(
#       game_id = stringr::str_replace(game_id, "tpg", ""),
#       game_id = as.numeric(game_id)
#     )
#   return (df)
# }
#
# imported_results_mini <- import_results_mini()
# imported_results <- import_results()
# imported_goals_df <- import_goals_df()
# imported_players_df <- import_players_df()

output_seasons_plot <- function(selected_seasons, chosen_plot) {
    pos <- imported_results_mini %>%
      dplyr::rename(
        game_no = pld,
        league_pos = ranking
      ) %>%
      dplyr::select(season, game_no, league_pos, pts)

  res <- imported_results %>%
    dplyr::rename(game_no = ssn_comp_game_no) %>%
    dplyr::filter(game_type == "League")

  df <- dplyr::inner_join(res, pos, by = c("season", "game_no")) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      opposition,
      venue,
      outcome,
      score,
      goals_for,
      goals_against,
      competition,
      league_tier,
      manager,
      league_pos,
      pts,
      attendance
    ) %>%
    dplyr::mutate(
      game_date = as.Date(game_date, format = '%d/%m/%Y'),
      ppg = pts / game_no
    )

    plot_df = df %>%
      dplyr::filter(season %in% selected_seasons) %>%
      dplyr::group_by(season)
    # %>%
    #   dplyr::mutate(game_date = as.character(game_date, format = "%e %B %Y"))

    # Set lowest league position for League Position chart
    if(max(df$season) %in% selected_seasons) {
      max_pos = 24 # Current season
    } else if("2019/20" %in% selected_seasons) {
      max_pos = 23 # Season ended early and Bury didn't compete
    } else {
      max_pos = (max(plot_df$game_no) / 2) + 1
    }

    # Set Y-axis based on chart type
    if(chosen_plot == "league_pos") {
      y_scale_var = ggplot2::scale_y_reverse(limits = c(max_pos, 1), breaks = c(1, 3, 7, 21, max_pos))
    } else if(chosen_plot == "ppg") {
      y_scale_var = ggplot2::scale_y_continuous(limits = c(0, 3), breaks = c(0, 1, 2, 3))
    } else {
      y_scale_var = ggplot2::scale_y_continuous()
    }

    plot_types <- c(
      "League Position" = "league_pos",
      "Total points" = "pts",
      "Points-per-game" = "ppg"
      )

    # Create the plot
    p = ggplot2::ggplot(
      plot_df,
      ggplot2::aes(
        x = game_no,
        y = get(chosen_plot),
        group = 1,
        text = sprintf("Season: %s\nGame No: %.0f\nDate: %s\nOpposition: %s\nVenue: %s\nScore: %s\nLeague Tier: %.0f (%s)\nLeague Pos: %.0f\nTotal Points: %.0f\nPPG: %.2f\nManager: %s\nAttendance: %s", season, game_no, game_date, opposition, venue, score, league_tier, competition, league_pos, pts, ppg, manager, attendance)
               ),
      ) +
      ggplot2::geom_line(ggplot2::aes(color = season)) +
      ggplot2::geom_point(ggplot2::aes(color = season)) +
      ggplot2::labs(
        x = "Game no.",
        y = names(plot_types)[plot_types == chosen_plot]
      ) +
      y_scale_var +
      ggplot2::scale_color_brewer(name = "", palette = "Paired") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        text = ggplot2::element_text(size=8),
        axis.text = ggplot2::element_text(size=7),
        plot.caption = ggplot2::element_text(hjust=0)
      )

    output_p <- plotly::ggplotly(p, tooltip="text")

    return (output_p)
}
