man_summary <- function(selected_manager) {
  results_dataset %>%
    dplyr::filter(
      manager == selected_manager
    ) %>%
    dplyr::group_by(
      generic_comp
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      win_pc = W / P
    ) %>%
    dplyr::mutate(
      generic_comp = factor(generic_comp, levels = c(
          "Football League",
          "Non-League",
          "FA Cup",
          "League Cup",
          "Associate Members' Cup"
        )
      )
    ) %>%
    dplyr::arrange(
      generic_comp
    )
}

man_summary("Ian Dawes")
