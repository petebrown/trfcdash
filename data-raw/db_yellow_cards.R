yellow_cards <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/yellow_cards.csv",
  show_col_types = FALSE
)

usethis::use_data(yellow_cards, overwrite = TRUE)
