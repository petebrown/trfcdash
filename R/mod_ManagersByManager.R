#' ManagersByManager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ManagersByManager_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Managers By Manager shit goes HERE!")
  )
}

#' ManagersByManager Server Functions
#'
#' @noRd
mod_ManagersByManager_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
