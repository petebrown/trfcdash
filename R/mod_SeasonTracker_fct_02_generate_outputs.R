output_ssn_records <- function(selected_seasons) {
  records_df <- get_ssn_results(selected_seasons) %>%
  dplyr::filter(game_type == "League") %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(
    P = dplyr::n(),
    W = sum(outcome == "W"),
    D = sum(outcome == "D"),
    L = sum(outcome == "L"),
    GF = sum(goals_for),
    GA = sum(goals_against),
    GD = GF - GA,
    Pts = (W * 3) + D
  ) %>%
  dplyr::rename(Season = season)

  return (records_df)
}

get_streaks <- function(selected_seasons) {
  streaks <- imported_results %>%
    dplyr::filter(season %in% selected_seasons) %>%
    dplyr::arrange(season, ssn_game_no) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
    ) %>%
    dplyr::rename(Season = season) %>%
    dplyr::summarize(
      "Wins" = max(w_streak),
      "Unbeaten" = max(unbeaten_streak),
      "Losses" = max(losing_streak),
      "Winless" = max(winless_streak),
      "Draws" = max(d_streak),
      "Clean sheets" = max(clean_sheets))

  return (streaks)
}

get_ssn_results <- function(selected_seasons) {
  results_df <- imported_results %>%
    dplyr::filter(season %in% selected_seasons)

  return (results_df)
}

get_formatted_results <- function(selected_seasons) {
  results_df <-  get_ssn_results(selected_seasons) %>%
    dplyr::select(
      season,
      ssn_game_no,
      game_date,
      opposition,
      venue,
      score,
      competition,
      manager,
      attendance
    ) %>%
    dplyr::rename(
      Season = season,
      "Game \nNo" = ssn_game_no,
      Date = game_date,
      Opponent = opposition,
      Venue = venue,
      Score = score,
      Division = competition,
      Manager = manager,
      Attendance = attendance
    )

  return (results_df)
}

output_ssn_results <- function(season) {
  DT::renderDataTable(
    get_formatted_results(season),
    selection = 'single',
    filter = 'bottom',
    rownames = FALSE,
    options = list(
      paging = TRUE,
      pageLength = 10,
      info = TRUE,
      scrollX = TRUE,
      dom = 'frtip',
      columnDefs = list(
        list(targets = c(0, 1, 2, 3, 4, 6, 7), className = 'dt-left'),
        list(targets = c(5), className = 'dt-center'),
        list(targets = c(8), className = 'dt-right'))
    )
  )
}




















