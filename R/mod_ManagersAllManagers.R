#' ManagersAllManagers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ManagersAllManagers_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("ManagersAll Managers shit goes HERE!")
  )
}

#' ManagersAllManagers Server Functions
#'
#' @noRd
mod_ManagersAllManagers_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
