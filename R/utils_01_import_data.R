#' utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

all_seasons <- function() {
  seasons <- get_season_list()

  return (seasons)
}

filter_ssn_results <- function(selected_seasons) {
  results_df <- results_dataset %>%
    dplyr::filter(season %in% selected_seasons)

  return (results_df)
}


map_club_to_crest <- function(club_name) {

  img_path <- clubs_crests %>%
    dplyr::filter(club == club_name) %>%
    dplyr::pull(file_path)

  return (img_path)
}

map_competition_to_generic_logo <- function(comp) {
  df <- results_dataset %>%
    dplyr::select(
      competition,
      generic_comp
    ) %>%
    unique() %>%
    dplyr::arrange(competition) %>%
    dplyr::left_join(
      comp_logos,
      by = "generic_comp"
    )

  logo_path <- df %>%
    dplyr::filter(competition == comp) %>%
    dplyr::pull(file_path)

  return (logo_path)
}

map_competition_to_specific_logo <- function(comp, ssn) {

  logo_path <- spec_comp_logos %>%
    dplyr::filter(
      season == ssn,
      competition == comp
    ) %>%
    dplyr::pull(file_path_2)

  return (logo_path)
}

map_plr_to_img <- function(player) {

  img_path <- player_imgs %>%
    dplyr::filter(pl_index == player) %>%
    dplyr::pull(file_path)

  return (img_path)
}
