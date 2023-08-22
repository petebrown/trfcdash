yellow_cards <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/yellow_cards.csv",
  # col_select = -"min_yc",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    yellow_cards = dplyr::n()
  )

usethis::use_data(yellow_cards, overwrite = TRUE)
