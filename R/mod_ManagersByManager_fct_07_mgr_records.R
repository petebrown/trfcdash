output_mgr_records <- function(selected_manager, record_type, inc_cup_games, pens_as_draw) {

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


  if (record_type == "season") {
    df <- df %>%
      dplyr::group_by(
        season
      )
  } else if (record_type == "competition") {
    df <- df %>%
      dplyr::group_by(
        competition
      )
  } else if (record_type == "opposition") {
    df <- df %>%
      dplyr::group_by(
        opposition
      )
  }

  df <- df %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA
    ) %>%
    dplyr::mutate(
      win_pc = (W / P)
    ) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_to_title(.x),
      .cols = dplyr::contains(c("opposition", "season", "competition"))
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    compact = TRUE,
    style = list(
      fontWeight = 400,
      color = "black"
    ),
    rowStyle = function() {
      list(
        fontWeight = 300,
        color = "black"
      )
    },
    columns = c(
      list(
        GD = reactable::colDef(
          cell = function(value) {
            format_gd(value)
          }
        ),
        win_pc = reactable::colDef(
          name = "Win %",
          format = reactable::colFormat(
            percent = TRUE,
            digits = 1
          )
        )
      )
    )
  )

}
