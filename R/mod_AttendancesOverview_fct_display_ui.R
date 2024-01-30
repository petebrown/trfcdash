# Function to create AttendancesOverview sidebar based on inputs from AttendancesOverview_sidebar_ui
AttendancesOverview_sidebar <- function() {
  bslib::page_fluid(
    mod_AttendancesOverview_sidebar_ui("AttendancesOverview_sidebar_ui_1")
  )
}
