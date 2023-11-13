output_all_mgr_streaks <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- results_dataset %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome,
      )
    ) %>%
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
    generate_streaks(drop_games_played = FALSE) %>%
    dplyr::filter(
      P >= min_games,
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
      defeats,
      winless,
      draws,
      clean_sheets,
      wins_to_nil
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
      defeats = reactable::colDef(
        name = "Defeats",
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
      wins_to_nil = reactable::colDef(
        name = "Wins to nil",
        vAlign = "center"
      )
    )
  )

  return(output_tab)

}
