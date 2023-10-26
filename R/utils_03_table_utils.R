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

streaks_reactable <- function(df) {
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
