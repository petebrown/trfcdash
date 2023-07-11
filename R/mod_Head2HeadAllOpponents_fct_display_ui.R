# Function to create Head-to_head sidebar based on inputs from Head2HeadAllOpponents_sidebar_ui
Head2HeadAllOpponents_sidebar <- function() {
  bslib::page_fluid(
    mod_Head2HeadAllOpponents_sidebar_ui("Head2HeadAllOpponents_sidebar_ui_1")
  )
}
