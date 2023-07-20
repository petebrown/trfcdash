lge_tables_eos <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/eos_tables.csv",
  show_col_types = FALSE,
  col_select = -("url")
)

lge_tables <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/lge_tables.csv",
  show_col_types = FALSE,
  col_select = -("url")
)

lge_tables_by_venue <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/lge_tables_venue.csv",
  show_col_types = FALSE,
  col_select = -("url")
)

usethis::use_data(lge_tables, lge_tables_eos, lge_tables_by_venue, overwrite = TRUE)
