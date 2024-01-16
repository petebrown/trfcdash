get_otd_birthdays <- function(otd_date, inc_year = "No") {

  df <- player_info %>%
    dplyr::group_by(
      pl_index
    ) %>%
    dplyr::slice_head(
      n = 1
    ) %>%
    dplyr::filter(
      lubridate::month(player_dob) == lubridate::month(otd_date),
      lubridate::day(player_dob) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(player_dob) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(player_dob)
    ) %>%
    dplyr::select(
      player_name,
      player_dob,
      pl_index
    )


  reactable::reactable(
    data = df,
    columns = list(
      player_name = reactable::colDef(
        name = "Player"
      ),
      player_dob = reactable::colDef(
        name = "Date of Birth",
        format = reactable::colFormat(
          date = TRUE
        )
      ),
      pl_index = reactable::colDef(
        show = FALSE
      )
    )
  )

}
