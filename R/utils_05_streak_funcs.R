#' 05_streak_funcs
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_streak_matches <- function(df, streak_col = "wins") {
  # Calculate the rle (run-length encoding) of the wins column
  streaks <- rle(df[[streak_col]])

  # Find the maximum streak length
  max_streak_length <- max(streaks$lengths[streaks$values == 1])

  # Identify the positions of the longest streaks
  streak_positions <- which(streaks$values == 1 & streaks$lengths == max_streak_length)

  # Find the starting index of the longest streak in the original dataframe
  streak_start <- sapply(streak_positions, function(pos) {
    if (pos == 1) {
      1
    } else {
      cumsum(streaks$lengths)[pos - 1] + 1
    }
  })

  # Create a sequence of row indices for the longest streak
  longest_streak_indices <- unlist(lapply(streak_start, function(start) seq(start, length.out = max_streak_length)))

  # Filter the dataframe for the longest streak
  df_longest_streak <- df[longest_streak_indices, ]

  # Print the filtered dataframe
  df_longest_streak %>% dplyr::select(season:outcome)
}
