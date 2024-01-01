get_oldest_players <- function(n_records) {

  df <- player_apps %>%
    dplyr::filter(
      !is.na(player_dob)
    ) %>%
    dplyr::mutate(
      plr_game_age = game_date - player_dob
    ) %>%
    dplyr::select(
      menu_name,
      plr_game_age,
      game_date
    ) %>%
    dplyr::group_by(
      menu_name
    ) %>%
    dplyr::slice_max(
      order_by = plr_game_age,
      n = ifelse(n_records == "all", length(.$menu_name), n_records)
    ) %>%
    dplyr::mutate(
      max_age_yrs = floor(as.numeric(plr_game_age / 365.25)),
      max_age_days = ceiling(plr_game_age - (max_age_yrs * 365.25)),
      game_date = max(game_date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      dplyr::desc(plr_game_age)
    ) %>%
    dplyr::mutate(
      rank = dplyr::min_rank(dplyr::desc(plr_game_age))
    ) %>%
    dplyr::select(
      rank,
      menu_name,
      plr_game_age,
      game_date,
      max_age_yrs,
      max_age_days
    )

  reactable::reactable(
    data = df,
    style = "font-size: smaller;",
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "plr_game_age",
    columns = list(
      rank = reactable::colDef(
        name = "",
        width = 50
      ),
      menu_name = reactable::colDef(
        name = "Player",
        minWidth = 150
      ),
      plr_game_age = reactable::colDef(
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
      game_date = reactable::colDef(
        name = "Game Date",
        align = "right",
        format = reactable::colFormat(
          date = TRUE
        ),
      ),
      max_age_yrs = reactable::colDef(
        show = FALSE
      ),
      max_age_days = reactable::colDef(
        show = FALSE
      )
    )
  )

}
