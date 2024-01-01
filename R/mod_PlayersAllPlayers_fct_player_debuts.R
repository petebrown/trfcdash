get_pl_debuts <- function() {

  df <- player_apps %>%
    dplyr::filter(
      !is.na(player_dob)
    ) %>%
    dplyr::mutate(
      plr_game_age_days = as.numeric(game_date - player_dob)
    ) %>%
    dplyr::select(
      menu_name,
      game_date,
      plr_game_age_days
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::summarise(
      debut_age = min(plr_game_age_days),
      debut_date = min(game_date)
    ) %>%
    dplyr::mutate(
      debut_age_yrs = floor(debut_age / 365.25),
      debut_age_days = ceiling(debut_age - (debut_age_yrs * 365.25))
    ) %>%
    dplyr::ungroup() %>%
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
    style = "font-size: smaller;",
    searchable = TRUE,
    defaultSortOrder = "asc",
    defaultSorted = "debut_age",
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
