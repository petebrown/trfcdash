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
      game_date,
      plr_game_age
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
      max_age_days = ceiling(plr_game_age - (max_age_yrs * 365.25))
    ) %>%
    dplyr::arrange(
      dplyr::desc(plr_game_age)
    )

  reactable::reactable(df)

}
