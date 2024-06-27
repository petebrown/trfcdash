get_biggest_defeats <- function(year_range, league_tiers, includePlayOffs, cup_comps, venue_options, game_range, min_diff=4) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  min_game_no <- game_range[1]
  max_game_no <- game_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      ssn_year >= min_year,
      ssn_year <= max_year
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options,
      game_no >= min_game_no,
      game_no <= max_game_no
    )

  df <- df %>%
    dplyr::filter(
      outcome == "L",
      goal_diff <= -min_diff
    ) %>%
    dplyr::arrange(
      goal_diff,
      dplyr::desc(goals_against),
      game_date
    ) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      venue,
      opposition,
      score,
      competition,
      attendance,
      manager
    )

  reactable::reactable(
    data=df,
    class = "reactable-text",
    style = "font-size: smaller",
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    fullWidth = TRUE,
    compact = TRUE,
    searchable = TRUE,
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
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        )
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
          venue <- df$venue[index]

          club_and_crest(value, venue)
        },
        style = function(value, index) {
          if (df$venue[index] == "H") {
            font_weight = "450"
          } else {
            font_weight = "300"
          }
          list(
            fontWeight = font_weight
          )
        }
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
          season <- df$season[index]
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
      )
    )
  )
}
