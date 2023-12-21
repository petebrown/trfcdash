#' 03_table_utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Function to add plus sign (+) before positive figures
format_gd <- function(value) {
  if (value != 0)
    sprintf("%+3d", value)
  else
    value
}

generate_streaks <- function(df, drop_games_played = TRUE) {
  streaks <- df %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      blanks = ifelse(goals_for == 0, 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      defeats_to_0 = ifelse(outcome == "L" & goals_for == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      blanks_streak = ifelse(blanks == 0, 0, sequence(rle(as.character(blanks))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
      wins_to_nil = ifelse(wins_cs == 0, 0, sequence(rle(as.character(wins_cs))$lengths)),
      defeats_to_nil = ifelse(defeats_to_0 == 0, 0, sequence(rle(as.character(defeats_to_0))$lengths))
    ) %>%
    dplyr::summarize(
      P = dplyr::n(),
      wins = max(w_streak),
      unbeaten = max(unbeaten_streak),
      clean_sheets = max(clean_sheets),
      wins_to_nil = max(wins_to_nil),
      draws = max(d_streak),
      defeats = max(losing_streak),
      winless = max(winless_streak),
      Blanks = max(blanks_streak),
      defeats_to_nil = max(defeats_to_nil),
      .groups = "drop"
    )

  # Remove P column if drop_games_played is TRUE
  if (drop_games_played) {
    streaks <- streaks %>%
      dplyr::select(-P)
  }

  return(streaks)

}

format_streak_cols <- function() {
  list(
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
}

streaks_reactable <- function(df) {
  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    columns = format_streak_cols()
  )
}

generate_record <- function(df) {
  df %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(goals_for) - sum(goals_against),
      win_pc = W / P,
      Pts = (sum(game_type == "League" & outcome == "W") * 3) + sum(game_type == "League" & outcome == "D"),
      PPG = Pts / sum(game_type == "League"),
      .groups = "drop"
    )
}


# Javascript function to return the total of a column in a Reactable table
js_total_col <- function() {
  reactable::JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }"
    )
}
