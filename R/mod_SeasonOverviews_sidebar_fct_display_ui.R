# Function to create SeasonOverviews sidebar based on inputs from SeasonOverviews_sidebar_ui
SeasonOverviews_sidebar <- function() {
  bslib::page_fluid(
    mod_SeasonOverviews_sidebar_ui("SeasonOverviews_sidebar_ui_1")
  )
}
