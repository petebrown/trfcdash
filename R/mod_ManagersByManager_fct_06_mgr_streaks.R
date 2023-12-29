output_mgr_streaks <- function(selected_manager, streak_type) {

  df <- results_dataset %>%
    dplyr::filter(
      manager == selected_manager
    )

  if (streak_type == "season") {
    df <- df %>%
      dplyr::group_by(
        season
      )
  }

  if (streak_type == "opposition") {
    df <- df %>%
      dplyr::group_by(
        opposition
      )
  }

  df <- df %>%
    generate_streaks()

  reactable::reactable(
    data = df,
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "wins",
    columns = c(
      format_streak_cols()
    )
  )

}
