get_eos_table <- function(selected_seasons) {
  res <- filter_ssn_results(selected_seasons) %>%
    dplyr::select(game_date, league_tier)

  final_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    ) %>%
    dplyr::left_join(
      res,
      by = "game_date"
    )
}

get_lge_tables <- function(selected_seasons) {
  res <- filter_ssn_results(selected_seasons) %>%
    dplyr::select(game_date, league_tier)

  lge_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    ) %>%
    dplyr::left_join(
      res,
      by = "game_date"
    )
}

output_ssn_reactable <- function(selected_seasons, n_fixtures) {
  top_level <- results_dataset %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      opposition,
      venue,
      outcome,
      score,
      competition,
      attendance,
      manager,
      league_tier
    ) %>%
    dplyr::arrange(
      season,
      game_no
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
      season = reactable::colDef(name = "Season", minWidth = 100),
      game_no = reactable::colDef(name = "Game\nNo.", align = "left", minWidth = 80),
      game_date = reactable::colDef(name = "Date", minWidth = 120,
                                    format = reactable::colFormat(date = TRUE, locales = "en-GB")),
      venue = reactable::colDef(name = "Venue", minWidth = 80),
      opposition = reactable::colDef(name = "Opponent", minWidth = 180),
      outcome = reactable::colDef(name = "Res", align = "left", minWidth = 70),
      score = reactable::colDef(name = "Score", minWidth = 80),
      competition = reactable::colDef(name = "Competition", minWidth = 130),
      attendance = reactable::colDef(name = "Att.", minWidth = 110,
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
            defaultPageSize = 16,
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
              pos = reactable::colDef(name = "Pos", align = "left", width = 52),
              Team = reactable::colDef(width = 145),
              Pld = reactable::colDef(width = 50),
              W = reactable::colDef(width = 40),
              D = reactable::colDef(width = 40),
              L = reactable::colDef(width = 40),
              GF = reactable::colDef(width = 50),
              GA = reactable::colDef(width = 50),
              Pts = reactable::colDef(width = 50),
              league_tier = reactable::colDef(show = FALSE)
            ),
            rowStyle = function(index) {
              tier = lge_tab[index, "league_tier"]
              season = lge_tab[index, "season"]
              pos = lge_tab[index, "pos"]
              team = lge_tab[index, "Team"]

              styles = list()
              if (team == "Tranmere Rovers") {
                styles = c(styles, fontWeight = "bold")
              }

              if (
                tier %in% c(2, 3, 5) & pos >= 21 |
                tier == 4 & pos >= 23
                ) {
                  styles = c(styles, background = "rgba(0, 0, 0, 0.03)")
              }

              if (
                tier %in% c(2, 3) & (pos %in% c(2, 6)) |
                tier == 4 & (pos %in% c(3, 7)) |
                tier == 5 & (pos %in% c(1, 3, 7))
                ) {
                styles = c(styles, borderBottom = "1px solid rgba(0, 0, 0, 0.1)")
              }

              return(styles)
            }
          )
        )
      )
    }
  )

}

