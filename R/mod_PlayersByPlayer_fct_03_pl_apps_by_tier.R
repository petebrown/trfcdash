output_pl_summary_by_tier <- function(inp_player_name) {
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
      by = c("game_date", "game_no", "season")
    ) %>%
    dplyr::filter(
      game_type == "League"
    ) %>%
    dplyr::group_by(
      league_tier,
      season
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
      league_tier = dplyr::case_match(
        league_tier,
        2 ~ "2 - Championship",
        3 ~ "3 - League One",
        4 ~ "4 - League Two",
        5 ~ "5 - National League"
      ),
      mins_per_gl = dplyr::case_when(
        mins_played / Goals != Inf ~ mins_played / Goals,
        TRUE ~ NA
      ),
      games_per_gl = mins_per_gl / 90,
      win_pc = W / P
    ) %>%
    dplyr::arrange(
      league_tier
    ) %>%
    dplyr::select(
      league_tier,
      season,
      P,
      starts,
      sub_apps,
      W,
      D,
      L,
      win_pc,
      mins_played,
      Goals,
      mins_per_gl,
      games_per_gl
    ) %>%
    dplyr::select(
      where(
        ~sum(!is.na(.x)) > 0
      )
    )

  reactable::reactable(
    data = df,
    pagination = FALSE,
    groupBy = c("league_tier"),
    defaultColDef = reactable::colDef(
      aggregate = "sum"
    ),
    columns = list(
      win_pc = reactable::colDef(
        name = "Win %",
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        ),
        aggregate = reactable::JS("function(values, rows) {
            let games_played = 0
            let wins = 0
            rows.forEach(function(row) {
              games_played += row['P']
              wins += row['W']
            })
            return wins / games_played
        }")
      ),
      mins_per_gl = reactable::colDef(
        format = reactable::colFormat(
          digits = 0
        ),
        aggregate = reactable::JS("function(values, rows) {
          let mins_played = 0
          let goals = 0
          rows.forEach(function(row) {
            mins_played += row['mins_played']
            goals += row['Goals']
          })
          return mins_played / goals
        }")
      ),
      games_per_gl = reactable::colDef(
        format = reactable::colFormat(
          digits = 1
        ),
        aggregate = reactable::JS("function(values, rows) {
          let mins_played = 0
          let goals = 0
          rows.forEach(function(row) {
            mins_played += row['mins_played']
            goals += row['Goals']
          })
          return mins_played / goals / 90
        }")
      )
    )
  )
}
