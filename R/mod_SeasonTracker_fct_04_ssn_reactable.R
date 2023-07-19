get_eos_table <- function(selected_seasons) {
  final_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    )
}

get_lge_tables <- function(selected_seasons) {
  lge_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    )
}

output_ssn_reactable <- function(selected_seasons, n_fixtures) {
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
      manager,
      league_tier
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

  lge_tabs <- get_lge_tables(selected_seasons)

  reactable::reactable(
    data = top_level,
    defaultPageSize = n_fixtures,
    fullWidth = TRUE,
    compact = TRUE,
    searchable = TRUE,
    borderless = TRUE,
    filterable = FALSE,
    striped = TRUE,
    resizable = TRUE,
    columns = list(
      season = reactable::colDef(name = "Season", width = 100),
      ssn_game_no = reactable::colDef(name = "Game\nNo.", align = "left", width = 80),
      game_date = reactable::colDef(name = "Date", width = 120,
                                    format = reactable::colFormat(date = TRUE, locales = "en-GB")),
      venue = reactable::colDef(name = "Venue", width = 80),
      opposition = reactable::colDef(name = "Opponent", width = 180),
      outcome = reactable::colDef(name = "Res", align = "left", width = 70),
      score = reactable::colDef(name = "Score", width = 80),
      competition = reactable::colDef(name = "Competition", width = 130),
      attendance = reactable::colDef(name = "Att.", width = 110,
                                     format = reactable::colFormat(digits = 0, separators = TRUE)),
      manager = reactable::colDef(name = "Manager"),
      league_tier = reactable::colDef(show = FALSE)
    ),
    details = function(index) {
      line_up = second_level[second_level$game_date == top_level$game_date[index], ]
      lge_tab = lge_tabs[lge_tabs$game_date == top_level$game_date[index], ]
      bslib::layout_column_wrap(
        width = 1/2,
        div(
          class = "reactable-details",
          bslib::card_title("Line-up"),
          reactable::reactable(
            data = line_up,
            class = "reactable-text",
            outlined = FALSE,
            bordered = FALSE,
            borderless = TRUE,
            defaultPageSize = 14,
            compact    = TRUE,
            filterable = FALSE,
            resizable  = TRUE,
            columns    = list(
              season = reactable::colDef(show = FALSE),
              game_date = reactable::colDef(show = FALSE),
              shirt_no = reactable::colDef(name = "No.", align = "left", width = 60),
              player_name = reactable::colDef(name = "Player", width = 180),
              role = reactable::colDef(name = "Role", width = 60),
              mins_played = reactable::colDef(name = "Mins", width = 60),
              goals_scored = reactable::colDef(name = "Goals", width = 50),
              yellow_cards = reactable::colDef(name = "YC", width = 40),
              red_cards = reactable::colDef(name = "RC", width = 40)
            ),
            rowStyle = function(index) {
              if (line_up[index, "role"] == "sub") {
                list(
                  background = "rgba(0, 0, 0, 0.03)"
                )
              }
            }
          )
        ),
        div(
          class = "reactable-details",
          bslib::card_title("As It Stood"),
          reactable::reactable(
            data = lge_tab,
            class = "reactable-text",
            defaultPageSize = 24,
            compact    = TRUE,
            bordered = FALSE,
            borderless = TRUE,
            outlined = FALSE,
            filterable = FALSE,
            searchable = FALSE,
            resizable  = TRUE,
            columns    = list(
              season = reactable::colDef(show = FALSE),
              game_no = reactable::colDef(show = FALSE),
              game_date = reactable::colDef(show = FALSE),
              pos = reactable::colDef(name = "Pos", width = 50),
              Team = reactable::colDef(width = 145),
              Pld = reactable::colDef(width = 50),
              W = reactable::colDef(width = 40),
              D = reactable::colDef(width = 40),
              L = reactable::colDef(width = 40),
              GF = reactable::colDef(width = 50),
              GA = reactable::colDef(width = 50),
              Pts = reactable::colDef(width = 50)
            ),
            rowStyle = function(index) {
              tier = top_level[index, "league_tier"]
              season = lge_tab[index, "season"]
              pos = lge_tab[index, "pos"]
              team = lge_tab[index, "Team"]

              styles = list()
              if (team == "Tranmere Rovers") {
                styles = c(styles, fontWeight = "bold")
              }

              if (tier %in% c(2, 3, 5)) {
                if (pos >= 20) {
                  styles = c(styles, background = "rgba(0, 0, 0, 0.03)")
                }
              } else {
                if (pos >= 23) {
                  styles = c(styles, background = "rgba(0, 0, 0, 0.03)")
                }
              }

              return(styles)
            }
          )
        )
      )
    }
  )

}

