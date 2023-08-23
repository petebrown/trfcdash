# Function to create Head2Head sidebar based on inputs from Head2HeadByOpponent_sidebar_ui
Head2HeadByOpponent_sidebar <- function() {
  bslib::page_fluid(
    mod_Head2HeadByOpponent_sidebar_ui("Head2HeadByOpponent_sidebar_ui_1")
  )
}
