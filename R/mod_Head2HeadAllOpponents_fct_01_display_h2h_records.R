output_h2h_records <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {
  df <- results_dataset %>%
    dplyr::filter(
      ssn_year >= year_range[1],
      ssn_year <= year_range[2]
    ) %>%
    dplyr::filter(
      league_tier %in% league_tiers | generic_comp %in% cup_comps,
      dplyr::case_when(
        includePlayOffs == "No" ~ !grepl("play-off", competition, ignore.case = TRUE),
        TRUE ~ TRUE
      ),
      venue %in% venue_options
    ) %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::group_by(
      opposition
    ) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA
    ) %>%
    dplyr::mutate(
      win_pc = (W / P)
    ) %>%
    dplyr::filter(
      P >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(win_pc),
      P,
      opposition
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "win_pc",
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$opposition)),
    defaultColDef = reactable::colDef(
      vAlign = "center"
    ),
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    columns = list(
      opposition = reactable::colDef(
        name = "Opposition",
        minWidth = 130,
        cell = function(value) {
          club_and_crest(value)
        }
      ),
      P = reactable::colDef(
        minWidth = 50
      ),
      W = reactable::colDef(
        minWidth = 50
      ),
      D = reactable::colDef(
        minWidth = 50
      ),
      L = reactable::colDef(
        minWidth = 50
      ),
      GF = reactable::colDef(
        minWidth = 50
      ),
      GA = reactable::colDef(
        minWidth = 50
      ),
      GD = reactable::colDef(
        vAlign = "center",
        minWidth = 50,
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          sprintf("%+3d", value)
        }
      ),
      win_pc = reactable::colDef(
        name = "Win %",
        align = "right",
        vAlign = "center",
        minWidth = 150,
        defaultSortOrder = "desc",
        # Render the bar charts using a custom cell render function
        cell = function(value) {
          # Format as percentages with 1 decimal place
          value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
          bar_chart(
            value,
            width = value,
            fill = "lightblue",
            background = "#F2F2F2"
          )
        }
      )
    )
  )
}
