# Function to create Attendances By Season sidebar based on inputs from AttendancesBySeason_sidebar_ui
AttendancesBySeason_sidebar <- function() {
  bslib::page_fluid(
    mod_AttendancesBySeason_sidebar_ui("AttendancesBySeason_sidebar_ui_1")
  )
}
