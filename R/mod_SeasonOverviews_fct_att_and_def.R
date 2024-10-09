get_attack_and_defend <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  min_game_no <- game_range[1]
  max_game_no <- game_range[2]

  filtered_df <- results_dataset %>%
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
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    ) %>%
    dplyr::arrange(game_date)

  df <- filtered_df %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      game_no = dplyr::row_number()
    ) %>%
    dplyr::filter(
      game_no >= min_game_no,
      game_no <= max_game_no
    )

  team_df <- df %>%
    dplyr::summarise(
      played = dplyr::n(),
      scored = sum(goals_for > 0),
      av_gf = mean(goals_for),
      clean_sheets = sum(goals_against == 0),
      wins_to_nil = sum(goals_against == 0 & outcome == "W"),
      blanks = sum(goals_for == 0),
      av_ga = mean(goals_against)
    )

  scorers_df <- player_apps %>%
    dplyr::filter(
      goals_scored > 0,
      game_date %in% df$game_date
    ) %>%
    dplyr::select(
      season,
      menu_name
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarize(
      diff_scorers = dplyr::n_distinct(menu_name),
    )

  df <- team_df %>%
    dplyr::left_join(
      scorers_df,
      by = c("season")
    ) %>%
    dplyr::select(
      season,
      played,
      scored,
      av_gf,
      diff_scorers,
      blanks,
      clean_sheets,
      wins_to_nil,
      av_ga
    )

  reactable::reactable(
    df,
    searchable = TRUE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$season)),
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    defaultSortOrder = "desc",
    defaultSorted = "season",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    columns = list(
      season = reactable::colDef(
        name = "Season",
        width = 75
      ),
      played = reactable::colDef(
        show = FALSE
      ),
      scored = reactable::colDef(
        name = "Scored 1+",
        align = "center"
      ),
      av_gf = reactable::colDef(
        name = "Av. Goals For",
        align = "center",
        format = reactable::colFormat(
          digits = 1
        )
      ),
      diff_scorers = reactable::colDef(
        name = "Different Scorers",
        align = "center"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets",
        align = "center"
      ),
      wins_to_nil = reactable::colDef(
        name = "Wins to Nil",
        align = "center"
      ),
      blanks = reactable::colDef(
        name = "Blanks",
        align = "center"
      ),
      av_ga = reactable::colDef(
        name = "Av. Goals Against",
        align = "center",
        format = reactable::colFormat(
          digits = 1
        )
      )
    )
  )
}
