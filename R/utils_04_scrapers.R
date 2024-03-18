#' 04_scrapers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_tab_11v11 <- function() {
  url <- rvest::read_html("https://www.11v11.com/league-tables/league-two/2024/")

  tab <- url %>%
    rvest::html_elements("table") %>%
    .[[1]] %>%
    rvest::html_table() %>%
    dplyr::mutate(
      Pos = dplyr::row_number()
    ) %>%
    dplyr::rename(
      pos = Pos,
      P = Pld
    ) %>%
    dplyr::select(
      -GD
    )

  return(tab)
}

get_tab_bbc <- function() {

  url <- rvest::read_html("https://www.bbc.com/sport/football/league-two/table")

  tab <- url %>%
    rvest::html_elements("table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::select(
      -c(
        'Goal Difference',
        dplyr::last_col()
      )
    ) %>%
    dplyr::rename(
      pos = Position,
      P = Played,
      W = Won,
      D = Drawn,
      L = Lost,
      GF = 'Goals For',
      GA = 'Goals Against',
      Pts = Points
    )

  return(tab)
}
