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

