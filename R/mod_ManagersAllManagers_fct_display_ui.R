# Function to create Managers (all managers) sidebar based on inputs from ManagersAllManagers_sidebar_ui
ManagersAllManagers_sidebar <- function() {
  bslib::page_fluid(
    mod_ManagersAllManagers_sidebar_ui("ManagersAllManagers_sidebar_ui_1")
  )
}
