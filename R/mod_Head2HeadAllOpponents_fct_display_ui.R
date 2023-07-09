# Function to create Season Tracker sidebar based on inputs from SeasonTracker_sidebar_ui
Head2HeadAllOpponents_sidebar <- function() {
  bslib::page_fluid(
    mod_Head2HeadAllOpponents_sidebar_ui("Head2HeadAllOpponents_sidebar_ui_1")
  )
}
