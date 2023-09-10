output_all_mgr_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, venue_options, min_games) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::filter(
      ssn_year >= min_year,
      ssn_year <= max_year,
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::group_by(
      manager
    ) %>%
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
      wins_to_0 = ifelse(wins_cs == 0, 0, sequence(rle(as.character(wins_cs))$lengths))
    ) %>%
    dplyr::summarize(
      games = dplyr::n(),
      wins = max(w_streak),
      unbeaten = max(unbeaten_streak),
      losses = max(losing_streak),
      winless = max(winless_streak),
      draws = max(d_streak),
      clean_sheets = max(clean_sheets),
      wins_to_0 = max(wins_to_0),
      .groups = "drop"
    ) %>%
    dplyr::filter(
      games >= min_games,
    ) %>%
    dplyr::mutate(
      mgr_name = manager
    ) %>%
    dplyr::arrange(
      dplyr::desc(wins)
    ) %>%
    dplyr::select(
      manager,
      mgr_name,
      wins,
      unbeaten,
      losses,
      winless,
      draws,
      clean_sheets,
      wins_to_0
    )

  output_tab <- reactable::reactable(
    data = df,
    defaultSorted = list("wins" = "desc"),
    columns = list(
      manager = reactable::colDef(
        name = "",
        width = 75,
        vAlign = "top",
        cell = function(value) {
          image <- img(
            src = dplyr::case_when(
              .default = paste0(
                "./www/images/managers/", tolower(gsub(' ', '-', value)), ".jpg"),
              value == "No manager" ~ "./www/images/crest.svg",
              stringr::str_detect(value, "Sheedy") ~ "./www/images/managers/kevin-sheedy.jpg",
              stringr::str_detect(value, "McAteer") ~ "./www/images/managers/jason-mcateer.jpg"
            ),
            style = dplyr::case_when(
              .default = "height: 50px; border-radius: 50%;",
              value == "No manager" ~ "height: 50px;"
            ),
            alt = value
          )
          tagList(
            div(style = "display: inline-block; width: 60px;", image)
          )
        }),
      mgr_name = reactable::colDef(
        name = "",
        vAlign = "center"
      ),
      wins = reactable::colDef(
        name = "Wins",
        vAlign = "center"
      ),
      unbeaten = reactable::colDef(
        name = "Unbeaten",
        vAlign = "center"
      ),
      losses = reactable::colDef(
        name = "Losses",
        vAlign = "center"
      ),
      winless = reactable::colDef(
        name = "Winless",
        vAlign = "center"
      ),
      draws = reactable::colDef(
        name = "Draws",
        vAlign = "center"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets",
        vAlign = "center"
      ),
      wins_to_0 = reactable::colDef(
        name = "Wins to nil",
        vAlign = "center"
      )
    )
  )

  return(output_tab)

}
