get_eos_table <- function(selected_seasons) {
  res <- filter_ssn_results(selected_seasons) %>%
    dplyr::select(game_date, league_tier)

  final_tables %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
    dplyr::mutate(
      GD = GF - GA
    ) %>%
    dplyr::relocate(
      GD,
      .after = GA
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
    dplyr::mutate(
      GD = GF - GA
    ) %>%
    dplyr::relocate(
      GD,
      .after = GA
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

output_ssn_reactable <- function(selected_seasons, inc_cup_games) {

    if (inc_cup_games == "No") {
    results <- results_dataset %>%
      dplyr::filter(
        game_type == "League"
      )
  } else {
    results <- results_dataset
  }

  top_level <- results %>%
    dplyr::filter(
      season %in% selected_seasons
    ) %>%
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
    ) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
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
      menu_name,
      role,
      mins_played,
      goals_scored,
      yellow_cards,
      red_cards
    )

  lge_tabs <- get_lge_tables(selected_seasons)

  reactable::reactable(
    data = top_level,
    class = "reactable-text",
    style = "font-size: smaller",
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(top_level$season)),
    fullWidth = TRUE,
    compact = TRUE,
    searchable = TRUE,
    borderless = TRUE,
    filterable = FALSE,
    resizable = TRUE,
    columns = list(
      season = reactable::colDef(
        name = "Season",
        width = 100
      ),
      game_no = reactable::colDef(
        name = "Game\nNo.",
        align = "left",
        width = 70
      ),
      game_date = reactable::colDef(
        name = "Date",
        width = 105,
        format = reactable::colFormat(date = TRUE, locales = "en-GB")
      ),
      venue = reactable::colDef(
        name = "Venue",
        width = 70,
        align = "center"
      ),
      opposition = reactable::colDef(
        name = "Opponent",
        minWidth = 200,
        cell = function(value, index) {
          venue <- top_level$venue[index]

          club_and_crest(value, venue)
        },
        style = function(value, index) {
          if (top_level$venue[index] == "H") {
            font_weight = "450"
          } else {
            font_weight = "300"
          }
          list(
            fontWeight = font_weight
          )
        }
      ),
      outcome = reactable::colDef(
        name = "Res",
        align = "center",
        minWidth = 45
      ),
      score = reactable::colDef(
        name = "Score",
        minWidth = 60,
        align = "center"
      ),
      competition = reactable::colDef(
        name = "Competition",
        minWidth = 200,
        vAlign = "center",
        cell = function(value, index) {
          # generic_comp_logo(value)

          season <- top_level$season[index]
          specific_comp_logo(value, season)
        }
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
    details = function(index) {
      line_up = second_level[second_level$game_date == top_level$game_date[index], ]
      lge_tab = lge_tabs[lge_tabs$game_date == top_level$game_date[index], ]
      bslib::layout_column_wrap(
        width = 1/2,
        fill=FALSE,
        fillable=FALSE,
        div(
          class = "reactable-details",
          bslib::card_title("Line-up"),
          reactable::reactable(
            data = line_up,
            class = "reactable-text",
            defaultColDef = reactable::colDef(
              vAlign = "center",
              headerClass = "bar-sort-header"
            ),
            showSortIcon = FALSE,
            outlined = FALSE,
            bordered = FALSE,
            borderless = TRUE,
            defaultPageSize = 16,
            compact = TRUE,
            filterable = FALSE,
            resizable = TRUE,
            columns = list(
              season = reactable::colDef(show = FALSE),
              game_date = reactable::colDef(show = FALSE),
              shirt_no = reactable::colDef(
                name = "No.",
                align = "center",
                min_Width = 35
              ),
              menu_name = reactable::colDef(
                name = "Player",
                minWidth = 200,
                cell = function(value) {
                  plr_name_and_headshot(value, img_size=30)
                }
              ),
              role = reactable::colDef(
                name="Role",
                align = "left",
                minWidth=75,
                cell = function(value) {
                  stringr::str_to_title(value)
                }
                ),
              mins_played = reactable::colDef(
                name = "Mins",
                align="center",
                minWidth = 50
              ),
              goals_scored = reactable::colDef(
                name = "Goals",
                align="center",
                minWidth = 50
              ),
              yellow_cards = reactable::colDef(
                name = "YC",
                align="center",
                minWidth = 32
              ),
              red_cards = reactable::colDef(
                name = "RC",
                align="center",
                minWidth = 32
              )
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
              showSortIcon = FALSE,
              defaultColDef = reactable::colDef(headerClass = "bar-sort-header"),
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
                GD = reactable::colDef(width = 50,
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

