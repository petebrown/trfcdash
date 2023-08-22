rc_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/red_cards.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    red_cards = dplyr::n()
  )


red_cards <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/red_cards.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    red_cards = dplyr::n()
  )


usethis::use_data(
  red_cards,

  overwrite = TRUE
)
