get_h2h_man_summary <- function(df) {

  df %>%
    dplyr::group_by(
      manager
    ) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(GF - GA),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mgr_name = manager,
      win_pc = W / P
    ) %>%
    dplyr::arrange(
      dplyr::desc(win_pc),
      dplyr::desc(P)
    ) %>%
    dplyr::select(
      manager,
      mgr_name,
      P,
      W,
      D,
      L,
      GF,
      GA,
      GD,
      win_pc
    )
}
