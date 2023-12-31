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
    dplyr::arrange(
      debut_age
    )

  reactable::reactable(df)

}
