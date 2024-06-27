get_pl_debuts <- function(year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games) {

  min_year <- year_range[1]
  max_year <- year_range[2]

  df <- player_apps %>%
    dplyr::left_join(
      results_dataset,
      by = c(
        "season",
        "game_date",
        "game_no"
      )
    ) %>%
    dplyr::mutate(
      ssn_year = as.numeric(stringr::str_sub(season, end = 4)),
      game_year = lubridate::year(game_date),
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
    dplyr::filter(
      !is.na(player_dob)
    ) %>%
    dplyr::mutate(
      plr_game_age_days = as.numeric(game_date - player_dob)
    ) %>%
    dplyr::select(
      menu_name,
      game_date,
      role,
      plr_game_age_days
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      debut_age = min(plr_game_age_days),
      debut_date = min(game_date)
    ) %>%
    dplyr::mutate(
      debut_age_yrs = floor(debut_age / 365.25),
      debut_age_days = ceiling(debut_age - (debut_age_yrs * 365.25))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      starts >= min_games
    ) %>%
    dplyr::arrange(
      dplyr::desc(debut_age)
    ) %>%
    dplyr::mutate(
      rank = dplyr::min_rank(debut_age)
    ) %>%
    dplyr::select(
      rank,
      menu_name,
      debut_age,
      debut_date,
      debut_age_yrs,
      debut_age_days
    )

  reactable::reactable(
    data = df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "asc",
    defaultSorted = "debut_age",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    columns = list(
      rank = reactable::colDef(
        name = "",
        width = 50
      ),
      menu_name = reactable::colDef(
        name = "Player",
        minWidth = 150
      ),
      debut_age = reactable::colDef(
        name = "Age",
        cell = function(value) {
          years = floor(as.numeric(value / 365.25))
          days = ceiling(value - (years * 365.25))

          sprintf(
            "%d years, %d days",
            years,
            days
          )
        },
      ),
      debut_date = reactable::colDef(
        name = "Game Date",
        align = "right",
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        ),
      ),
      debut_age_yrs = reactable::colDef(
        show = FALSE
      ),
      debut_age_days = reactable::colDef(
        show = FALSE
      )
    )
  )

}
