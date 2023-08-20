red_cards <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/red_cards.csv",
  show_col_types = FALSE
)

usethis::use_data(red_cards, overwrite = TRUE)
