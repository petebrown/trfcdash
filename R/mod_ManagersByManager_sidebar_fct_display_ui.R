# Function to create ManagersByManager sidebar based on inputs from ManagersByManager_sidebar_ui
ManagersByManager_sidebar <- function() {
  bslib::page_fluid(
    mod_ManagersByManager_sidebar_ui("ManagersByManager_sidebar_ui_1")
  )
}
