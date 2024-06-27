output_pl_summary_by_opp <- function(inp_player_name) {
  df <- player_apps %>%
    dplyr::filter(
      menu_name == inp_player_name
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
    dplyr::group_by(
      opposition
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
      opposition,
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
      Opposition = opposition,
      "Starts\n(sub)" = app_sums,
      "Win %" = win_pc,
      "Mins\nplayed" = mins_played
    )

  reactable::reactable(
    df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    columns = list(
      Opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 130,
        cell = function(value) {
          club_and_crest(value)
        }
      ),
      mins_per_gl = reactable::colDef(
        name = "Mins per goal",
        cell = function(value) {
          if (is.na(value)) {
            return ('-')
          } else {
            return (round(value, 1))
          }
        }
      ),
      games_per_gl = reactable::colDef(
        name = "Games per goal",
        cell = function(value) {
          if (is.na(value)) {
            return ('-')
          } else {
            return (round(value, 1))
          }
        }
      )
    )
  )
}
