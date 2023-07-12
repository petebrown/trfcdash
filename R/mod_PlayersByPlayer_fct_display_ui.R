# Function to create Players sidebar based on inputs from PlayersByPlayer_sidebar_ui
PlayersByPlayer_sidebar <- function() {
  bslib::page_fluid(
    mod_PlayersByPlayer_sidebar_ui("PlayersByPlayer_sidebar_ui_1")
  )
}
