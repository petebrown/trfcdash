get_streaks <- function(selected_seasons) {
  streaks <- filter_ssn_results(selected_seasons) %>%
    dplyr::arrange(season, game_no) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      defeats_to_0 = ifelse(outcome == "L" & goals_for == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
      wins_to_nil = ifelse(wins_cs == 0, 0, sequence(rle(as.character(wins_cs))$lengths)),
      defeats_to_nil = ifelse(defeats_to_0 == 0, 0, sequence(rle(as.character(defeats_to_0))$lengths))
    ) %>%
    dplyr::rename(Season = season) %>%
    dplyr::summarize(
      wins = max(w_streak),
      unbeaten = max(unbeaten_streak),
      clean_sheets = max(clean_sheets),
      wins_to_nil = max(wins_to_nil),
      draws = max(d_streak),
      defeats = max(losing_streak),
      winless = max(winless_streak),
      defeats_to_nil = max(defeats_to_nil)
    )

  return (streaks)
}

render_streaks <- function(selected_seasons) {
  df <- get_streaks(selected_seasons)

  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    columns = list(
      wins = reactable::colDef(
        name = "Wins"
      ),
      unbeaten = reactable::colDef(
        name = "Unbeaten"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets"
      ),
      wins_to_nil = reactable::colDef(
        name = "Wins to nil"
      ),
      draws = reactable::colDef(
        name = "Draws"
      ),
      defeats = reactable::colDef(
        name = "Defeats"
      ),
      winless = reactable::colDef(
        name = "Winless"
      ),
      defeats_to_nil = reactable::colDef(
        name = "Defeats to nil"
      )
    )
  )
}
