output_ssn_reactable <- function(selected_seasons) {
  top_level <- results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::select(
      season,
      ssn_game_no,
      game_date,
      opposition,
      venue,
      outcome,
      score,
      competition,
      attendance,
      manager
    )

  second_level <- player_apps %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::arrange(
      game_date,
      role,
      shirt_no
    ) %>%
    dplyr::select(
      season,
      game_date,
      shirt_no,
      player_name,
      role,
      mins_played,
      goals_scored,
      yellow_cards,
      red_cards
    )

  reactable::reactable(
    data = top_level,
    compact = TRUE,
    searchable = TRUE,
    filterable = FALSE,
    striped = TRUE,
    resizable = TRUE,
    columns = list(
      season = reactable::colDef(name = "Season", width = 80),
      ssn_game_no = reactable::colDef(name = "Game\nNo.", align = "left", width = 80),
      game_date = reactable::colDef(name = "Date", width = 120,
                                    format = reactable::colFormat(date = TRUE, locales = "en-GB")),
      venue = reactable::colDef(name = "Venue", width = 100),
      opposition = reactable::colDef(name = "Opponent", width = 200),
      outcome = reactable::colDef(name = "Outcome", align = "left", width = 100),
      score = reactable::colDef(name = "Score", width = 100),
      competition = reactable::colDef(name = "Comp"),
      attendance = reactable::colDef(name = "Att", width = 110,
                                     format = reactable::colFormat(digits = 0, separators = TRUE)),
      manager = reactable::colDef(name = "Manager")
    ),
    details = function(index) {
      sec_lvl = second_level[second_level$game_date == top_level$game_date[index], ]
      reactable::reactable(
        data = sec_lvl,
        class = "line_up_details",
        defaultPageSize = 14,
        compact    = TRUE,
        filterable = FALSE,
        bordered   = TRUE,
        resizable  = TRUE,
        columns    = list(
          season = reactable::colDef(show = FALSE),
          game_date = reactable::colDef(show = FALSE),
          shirt_no = reactable::colDef(name = "Shirt No.", align = "left", width = 100),
          player_name = reactable::colDef(name = "Player", width = 230),
          role = reactable::colDef(name = "Role", width = 100),
          mins_played = reactable::colDef(name = "Mins played", width = 100),
          goals_scored = reactable::colDef(name = "Goals", width = 100),
          yellow_cards = reactable::colDef(name = "YC", width = 100),
          red_cards = reactable::colDef(name = "RC", width = 100)
        ),
        rowStyle = function(index) {
          if (sec_lvl[index, "role"] == "sub") {
            list(
              # fontWeight = "bold",
              background = "rgba(0, 0, 0, 0.03)"
            )
          }
        }
      )
    }
  )

}

