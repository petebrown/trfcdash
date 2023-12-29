output_mgr_streaks <- function(selected_manager, streak_type, inc_cup_games, pens_as_draw) {

  df <- results_dataset %>%
    dplyr::filter(
      manager == selected_manager,
      dplyr::case_when(
        inc_cup_games == "No" ~ game_type == "League",
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    )

  # if (inc_cup_games == "No") {
  #   df <- df %>%
  #     dplyr::filter(game_type == "League")
  # }


  if (streak_type == "season") {
    df <- df %>%
      dplyr::group_by(
        season
      )
  } else if (streak_type == "opposition") {
    df <- df %>%
      dplyr::group_by(
        opposition
      )
  }

  df <- df %>%
    generate_streaks(drop_games_played = FALSE) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_to_title(.),
      .cols = dplyr::contains(c("opposition", "season"))
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "wins",
    columns = c(
      list(
        P = reactable::colDef(
          name = "(P)",
          format = reactable::colFormat(
            prefix = "(",
            suffix = ")"
          )
        )
      ),
      format_streak_cols()
    )
  )

}
