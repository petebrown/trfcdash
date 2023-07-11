# Function to create Players sidebar based on inputs from PlayersAllPlayers_sidebar_ui
PlayersAllPlayers_sidebar <- function() {
  bslib::page_fluid(
    mod_PlayersAllPlayers_sidebar_ui("PlayersAllPlayers_sidebar_ui_1")
  )
}
