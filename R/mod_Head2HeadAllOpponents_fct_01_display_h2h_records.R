output_h2h_records <- function(year_range) {
  print(year_range)

  df <- results_dataset %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4))
    ) %>%
      dplyr::filter(
        ssn_year >= year_range[1],
        ssn_year <= year_range[2]
      ) %>%
      # dplyr::filter(
      #   league_tier %in% input$leagueTiers | generic_comp %in% input$cupComps,
      #   venue %in% input$venueOptions
      # ) %>%
      dplyr::group_by(
        opposition
      ) %>%
      dplyr::summarize(
        P = dplyr::n(),
        W = sum(outcome == "W"),
        D = sum(outcome == "D"),
        L = sum(outcome == "L"),
        GF = sum(goals_for),
        GA = sum(goals_against)
      ) %>%
      dplyr::mutate(
        win_pc = round((W / P) * 100, 2)
      ) %>%
      # dplyr::filter(
      #   P >= input$minGames
      # ) %>%
      dplyr::arrange(
        dplyr::desc(win_pc),
        P,
        opposition
      ) %>%
      dplyr::rename(
        Opposition = opposition,
        "Win %" = win_pc
      )

  print(head(df))
  return(df)
}
