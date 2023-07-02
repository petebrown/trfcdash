# Function to create Season Tracker sidebar based on inputs from SeasonTracker_sidebar_ui
SeasonTracker_sidebar <- function() {
  bslib::page_fluid(
    mod_SeasonTracker_sidebar_ui("SeasonTracker_sidebar_ui_1")
  )
}
