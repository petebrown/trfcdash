final_tables <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/eos_tables.csv",
  show_col_types = FALSE,
  col_select = -("url")
)

lge_tables <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/lge_tables.csv",
  show_col_types = FALSE,
  col_select = -("url")
)

usethis::use_data(lge_tables, final_tables, overwrite = TRUE)
