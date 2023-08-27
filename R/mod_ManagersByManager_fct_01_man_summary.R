get_mgr_summary_by_ssn <- function(selected_manager) {
  table <- results_dataset %>%
    dplyr::mutate(
      competition = dplyr::case_when(
        league_tier == 2 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "Championship",
        league_tier == 2 & stringr::str_detect(competition, "Play-Off") ~ "Championship Play-Off",
        league_tier == 3 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "League One",
        league_tier == 3 & stringr::str_detect(competition, "Play-Off") ~ "League One Play-Off",
        league_tier == 4 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "League Two",
        league_tier == 4 & stringr::str_detect(competition, "Play-Off") ~ "League Two Play-Off",
        league_tier == 5 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "National League",
        league_tier == 5 & stringr::str_detect(competition, "Play-Off") ~ "National League Play-Off",
        .default = generic_comp
      )
    ) %>%
    dplyr::filter(
      manager == selected_manager,
    ) %>%
    dplyr::group_by(
      season,
      generic_comp,
      competition,
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      w_pc = ifelse(W / P == 0, NA, W / P),
      d_pc = ifelse(D / P == 0, NA, D / P),
      l_pc = ifelse(L / P == 0, NA, L / P),
      outcomes = list(c(w_pc, d_pc, l_pc)),
      goals = list(c(GF, GA)),
      .groups = "drop"
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

  table %>%
    dplyr::mutate(
      season = stringr::str_glue("### {season}")
    ) %>%
    dplyr::select(-w_pc, -d_pc, -l_pc, -goals, -generic_comp
    ) %>%
    gt::gt(
      groupname_col = "season",
      process_md = TRUE
    ) %>%
    gt::cols_align(
      columns = competition,
      align = "left"
    ) %>%
    gt::cols_label(
      competition = "",
      P = gt::md("**P**"),
      W = gt::md("**W**"),
      D = gt::md("**D**"),
      L = gt::md("**L**"),
      GF = gt::md("**GF**"),
      GA = gt::md("**GA**")
    ) %>%
    gtExtras::gt_plt_bar_stack(
      outcomes,
      width = 150,
      labels = c("Wins ", " Draws ", " Defeats"),
      palette= c("darkgreen", "#999999", "#CD4F39"),
      fmt_fn = scales::label_percent(accuracy = 1),
    ) %>%
    gt::tab_options(
      table.width = "100%",
      row_group.background.color = "grey98",
      row_group.padding = "0.85rem"
    )
}


get_mgr_summary_by_comp <- function(selected_manager) {
  table <- results_dataset %>%
    dplyr::mutate(
      competition = dplyr::case_when(
        league_tier == 2 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "Championship",
        league_tier == 2 & stringr::str_detect(competition, "Play-Off") ~ "Championship Play-Off",
        league_tier == 3 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "League One",
        league_tier == 3 & stringr::str_detect(competition, "Play-Off") ~ "League One Play-Off",
        league_tier == 4 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "League Two",
        league_tier == 4 & stringr::str_detect(competition, "Play-Off") ~ "League Two Play-Off",
        league_tier == 5 & stringr::str_detect(competition, "Play-Off", negate = TRUE) ~ "National League",
        league_tier == 5 & stringr::str_detect(competition, "Play-Off") ~ "National League Play-Off",
        .default = generic_comp
      )
    ) %>%
    dplyr::filter(
      manager == selected_manager
    ) %>%
    dplyr::group_by(
      generic_comp,
      competition,
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      w_pc = ifelse(W / P == 0, NA, W / P),
      d_pc = ifelse(D / P == 0, NA, D / P),
      l_pc = ifelse(L / P == 0, NA, L / P),
      outcomes = list(c(w_pc, d_pc, l_pc)),
      goals = list(c(GF, GA)),
      .groups = "drop"
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

  table %>%
    dplyr::select(-w_pc, -d_pc, -l_pc, -goals, -generic_comp
    ) %>%
    gt::gt() %>%
    gt::cols_align(
      columns = competition,
      align = "left"
    ) %>%
    gt::cols_label(
      competition = "",
      P = gt::md("**P**"),
      W = gt::md("**W**"),
      D = gt::md("**D**"),
      L = gt::md("**L**"),
      GF = gt::md("**GF**"),
      GA = gt::md("**GA**")
    ) %>%
    gtExtras::gt_plt_bar_stack(
      outcomes,
      width = 150,
      labels = c("Wins ", " Draws ", " Defeats"),
      palette= c("darkgreen", "#999999", "#CD4F39"),
      fmt_fn = scales::label_percent(accuracy = 1)
    ) %>%
    gt::tab_options(
      table.width = "100%"
    )
}


get_mgr_summary_overall <- function(selected_manager) {
  table <- results_dataset %>%
    dplyr::filter(
      manager == selected_manager
    ) %>%
    dplyr::group_by(
      manager
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      w_pc = ifelse(W / P == 0, NA, W / P),
      d_pc = ifelse(D / P == 0, NA, D / P),
      l_pc = ifelse(L / P == 0, NA, L / P),
      outcomes = list(c(w_pc, d_pc, l_pc)),
      .groups = "drop"
    ) %>%
    dplyr::select(-manager, -w_pc, -d_pc, -l_pc)

  table %>%
    gt::gt() %>%
    gt::cols_label(
      P = gt::md("**P**"),
      W = gt::md("**W**"),
      D = gt::md("**D**"),
      L = gt::md("**L**"),
      GF = gt::md("**GF**"),
      GA = gt::md("**GA**")
    ) %>%
    gtExtras::gt_plt_bar_stack(
      outcomes,
      width = 150,
      labels = c("Wins ", " Draws ", " Defeats"),
      palette= c("darkgreen", "#999999", "#CD4F39"),
      fmt_fn = scales::label_percent(accuracy = 1)
    ) %>%
    gt::tab_options(
      table.width = "100%"
    )
}
