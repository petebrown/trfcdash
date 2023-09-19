output_mgr_plr_records <- function(mgr_name) {

  df <- player_apps %>%
    dplyr::left_join(
      results_dataset,
      by = c(
        "season",
        "game_date",
        "game_no"
      )
    ) %>%
    dplyr::filter(
      manager == mgr_name
    ) %>%
    dplyr::group_by(
      player_name
    ) %>%
    dplyr::summarise(
      starts = sum(role == "starter"),
      wins = sum(role == "starter" & outcome == "W"),
      win_pc = wins / starts,
      goals = sum(goals_scored),
      subbed_off = sum(!is.na(off_for)),
      subbed_off_pc = subbed_off / starts,
      sub_apps = sum(role == "sub"),
      sub_apps_pc = sub_apps / (starts + sub_apps)
    ) %>%
    dplyr::arrange(
      dplyr::desc(subbed_off_pc)
    )

  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    defaultSorted = list("starts" = "desc"),

    columns = list(
      player_name = reactable::colDef(
        name = "Player",
        show = TRUE
      ),

      starts = reactable::colDef(
        name = "Starts",
        show = TRUE
      ),

      wins = reactable::colDef(
        name = "Wins",
        show = TRUE
      ),

      win_pc = reactable::colDef(
        name = "Win rate (starts)",
        show = TRUE,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      ),

      subbed_off = reactable::colDef(
        name = "Subbed off",
        show = TRUE
      ),

      subbed_off_pc = reactable::colDef(
        name = "Subbed off %",
        show = TRUE,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      ),

      sub_apps = reactable::colDef(
        name = "Sub apps",
        show = TRUE
      ),

      sub_apps_pc = reactable::colDef(
        name = "Sub apps %",
        show = TRUE,
        format = reactable::colFormat(
          percent = TRUE,
          digits = 1
        )
      )
    )
  )

}
