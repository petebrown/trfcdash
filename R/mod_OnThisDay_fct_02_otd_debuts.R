get_otd_debuts <- function(otd_date, inc_year = "No") {

  df <- player_apps %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::summarise(
      debut = min(game_date)
    ) %>%
    dplyr::filter(
      lubridate::month(debut) == lubridate::month(otd_date),
      lubridate::day(debut) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(debut) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    )

  reactable::reactable(
    data = df,
    columns = list(
      menu_name = reactable::colDef(
        name = "Player"
      ),
      debut = reactable::colDef(
        name = "Debut Date",
        format = reactable::colFormat(
          date = TRUE
        )
      )
    )
  )
}
