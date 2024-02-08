get_attack_and_defend <_ function() {
  scorers_df <- player_apps %>%
    dplyr::filter(
      goals_scored > 0
    ) %>%
    dplyr::select(
      game_date,
      menu_name,
      goals_scored
    )

  df <- results_dataset %>%
    dplyr::left_join(
      scorers_df,
      by = c("game_date")
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarise(
      played = dplyr::n(),
      scored = sum(goals_for > 0),
      av_gf = mean(goals_for),
      diff_scorers = dplyr::n_distinct(menu_name),
      blanks = sum(goals_for == 0),
      clean_sheets = sum(goals_against == 0),
      av_ga = mean(goals_against)
    )

  reactable::reactable(
    df,
    columns = list(
      played = reactable::colDef(
        show = FALSE
      ),
      scored = reactable::colDef(
        name = "Goals Scored",
        align = "center"
      ),
      scored = reactable::colDef(
        name = "Goals Scored",
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
      blanks = reactable::colDef(
        name = "Blanks",
        align = "center"
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets",
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
