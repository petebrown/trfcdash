# Function to create On This Day sidebar based on inputs from OnThisDay_sidebar_ui
OnThisDay_sidebar <- function() {
  bslib::page_fluid(
    mod_OnThisDay_sidebar_ui("OnThisDay_sidebar_ui_1")
  )
}
