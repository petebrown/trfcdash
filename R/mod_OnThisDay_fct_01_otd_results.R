get_otd_results <- function(otd_date, inc_year = "No", as_reactable = "Yes") {

  df <- results_dataset %>%
    dplyr::filter(
      lubridate::month(game_date) == lubridate::month(otd_date),
      lubridate::day(game_date) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(game_date) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      years_ago = lubridate::year(lubridate::now()) - lubridate::year(game_date)
    ) %>%
    dplyr::select(
      years_ago,
      game_date,
      season,
      game_no,
      opposition,
      venue,
      outcome,
      score,
      goals_for,
      goals_against,
      scorers,
      competition,
      league_tier,
      league_pos,
      attendance,
      manager
    )

  reactable_df <- reactable::reactable(
    data = df,
    compact = TRUE,
    striped = TRUE,
    pagination = FALSE,
    style = list(
      color = "black",
      fontSize = "smaller"
    ),
    columns = list(
      years_ago = reactable::colDef(
        name = "",
        align = "left",
        width = 100,
        format = reactable::colFormat(
          suffix = " years ago",
        ),
        style = list(
          fontWeight = 600
        )
      ),
      season = reactable::colDef(
        name = "Season",
        align = "left",
        width = 75
      ),
      game_date = reactable::colDef(
        name = "Date",
        align = "left",
        width = 85,
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        )
      ),
      game_no = reactable::colDef(
        name = "Game No.",
        align = "center",
        width = 70
      ),
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 120
      ),
      outcome = reactable::colDef(
        show = FALSE
      ),
      venue = reactable::colDef(
        name = "Venue",
        align = "center",
        width = 70
      ),
      score = reactable::colDef(
        name = "Score",
        align = "center",
        width = 70
      ),
      goals_for = reactable::colDef(
        show = FALSE
      ),
      goals_against = reactable::colDef(
        show = FALSE
      ),
      scorers = reactable::colDef(
        name = "Scorers",
        minWidth = 150
      ),
      competition = reactable::colDef(
        name = "Competition",
        minWidth = 140
      ),
      league_tier = reactable::colDef(
        name = "League Tier",
        align = "center",
        width = 75
      ),
      league_pos = reactable::colDef(
        name = "League Pos.",
        align = "center",
        width = 75
      ),
      attendance = reactable::colDef(
        name = "Att.",
        width = 80,
        format = reactable::colFormat(
          separators = TRUE
        )
      ),
      manager = reactable::colDef(
        name = "Manager",
        minWidth = 100
      )
    )
  )

  if (as_reactable == "Yes") {
    reactable_df
  } else {
    return(df)
  }
}
