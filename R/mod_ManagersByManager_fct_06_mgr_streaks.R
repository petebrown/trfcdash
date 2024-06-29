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


  if (streak_type %in% c("season", "opposition")) {
    df <- df %>%
      dplyr::group_by(
        !!(as.name(streak_type))
      )
  }

  df <- df %>%
    generate_streaks(drop_games_played = FALSE) %>%
    dplyr::rename_with(
      .fn = ~ stringr::str_to_title(.),
      .cols = dplyr::contains(c("opposition", "season"))
    )

  if (streak_type == "overall") {
    df <- df %>%
      dplyr::mutate(
        Overall = "Overall"
      ) %>%
      dplyr::select(
        Overall,
        everything()
      )
  }

  reactable::reactable(
    data = df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(df$P)),
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "wins",
    columns = c(
      list(
        P = reactable::colDef(
          show = FALSE
        ),
        Opposition = reactable::colDef(
          name = "Opposition",
          minWidth = 175,
          cell = function(value) {
            club_and_crest(value)
          }
        )
      ),
      format_streak_cols()
    )
  )

}
