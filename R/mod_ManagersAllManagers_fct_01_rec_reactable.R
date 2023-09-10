output_all_mgr_records <- function(year_range, league_tiers, includePlayOffs, cup_comps, venue_options, min_games) {

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
    dplyr::group_by(manager) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(GF - GA),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      mgr_name = manager,
      win_pc = round((W / P) * 100, 2)
    ) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(win_pc),
      P,
      manager
    ) %>%
    dplyr::select(
      manager,
      mgr_name,
      P,
      W,
      D,
      L,
      GF,
      GA,
      GD,
      win_pc
    )

  output_tab <- reactable::reactable(
    data = df,
    defaultSorted = list("win_pc" = "desc"),
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
      P = reactable::colDef(
        vAlign = "center"
      ),
      W = reactable::colDef(
        vAlign = "center"
      ),
      D = reactable::colDef(
        vAlign = "center"
      ),
      L = reactable::colDef(
        vAlign = "center"
      ),
      GF = reactable::colDef(
        vAlign = "center"
      ),
      GA = reactable::colDef(
        vAlign = "center"
      ),
      GD = reactable::colDef(
        vAlign = "center"
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        vAlign = "center"
      )
    )
  )

  return(output_tab)

}
