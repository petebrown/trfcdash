get_h2h_meetings <- function(opponent, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      opposition == opponent,
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    )

  selected_seasons <- unique(df$season)

  top_level <- df %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      venue,
      opposition,
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
    defaultPageSize = 10,
    fullWidth = TRUE,
    compact = TRUE,
    searchable = TRUE,
    borderless = TRUE,
    filterable = FALSE,
    resizable = TRUE,
    columns = list(
      season = reactable::colDef(
        name = "Season",
        minWidth = 100
      ),
      game_no = reactable::colDef(
        name = "Game\nNo.",
        align = "left",
        minWidth = 65
      ),
      game_date = reactable::colDef(
        name = "Date",
        minWidth = 105,
        format = reactable::colFormat(date = TRUE, locales = "en-GB")
      ),
      venue = reactable::colDef(
        name = "Venue",
        minWidth = 65,
        align = "center"
      ),
      opposition = reactable::colDef(
        name = "Opponent",
        minWidth = 180
      ),
      outcome = reactable::colDef(
        name = "Res",
        align = "left",
        minWidth = 45
      ),
      score = reactable::colDef(
        name = "Score",
        minWidth = 60,
        align = "center"
      ),
      competition = reactable::colDef(
        name = "Competition",
        minWidth = 110
      ),
      attendance = reactable::colDef(
        name = "Att.",
        minWidth = 70,
        format = reactable::colFormat(digits = 0, separators = TRUE)
      ),
      manager = reactable::colDef(
        name = "Manager",
        minWidth = 120,
        align = "right"
      ),
      league_tier = reactable::colDef(
        show = FALSE
      )
    ),
    rowClass = "results-row",
    rowStyle = function(index) {
      if (top_level[index, "venue"] == "H") {
        list(
          # background = "rgb(248, 251, 253)",
          fontWeight = "500"
        )
      }
    },
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
        if (!is.na(top_level[index, "league_tier"])) {
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
                GD = reactable::colDef(
                  width = 50,
                  # Function to add plus sign (+) before positive figures
                  cell = function(value) {
                    format_gd(value)
                  }
                ),
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
        } else {
          div()
        }
      )
    }
  )

}

