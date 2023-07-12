output_pl_summary_by_mgr <- function(inp_player_name) {
  player_apps %>%
    dplyr::filter(
      player_name == inp_player_name
    ) %>%
    tidyr::replace_na(
      list(
        goals_scored = 0,
        mins_played = 0,
        yellow_cards = 0,
        red_cards = 0,
        mins_played = 0
      )
    ) %>%
    dplyr::left_join(
      results_dataset,
      by = "game_date"
    ) %>%
    dplyr::filter(
      game_type == "League"
    ) %>%
    dplyr::group_by(
      manager
    ) %>%
    dplyr::summarise(
      P = dplyr::n(),
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      mins_played = sum(mins_played),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      Goals = sum(goals_scored)
    ) %>%
    dplyr::mutate(
      app_sums = stringr::str_glue("{starts} ({sub_apps})"),
      mins_per_gl = dplyr::case_when(
        mins_played / Goals != Inf ~ mins_played / Goals,
        TRUE ~ NA
      ),
      games_per_gl = round(mins_per_gl / 90, 2),
      mins_per_gl = round(mins_per_gl, 2),
      win_pc = round((W / P) * 100, 1)
    ) %>%
    dplyr::arrange(
      dplyr::desc(P)
    ) %>%
    dplyr::select(
      manager,
      P,
      app_sums,
      W,
      D,
      L,
      win_pc,
      mins_played,
      Goals,
      mins_per_gl,
      games_per_gl
    ) %>%
    dplyr::rename(
      Manager = manager,
      "Starts\n(sub)" = app_sums,
      "Win %" = win_pc,
      "Mins\nplayed" = mins_played,
      "Mins\nper goal" = mins_per_gl,
      "Games\nper goal" = games_per_gl
    ) %>%
    dplyr::select(
      where(
        ~sum(!is.na(.x)) > 0
      )
    )
}
