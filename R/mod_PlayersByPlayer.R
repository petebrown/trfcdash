#' PlayersByPlayer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PlayersByPlayer_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Players By Player shit goes HERE!")
  )
}

#' PlayersByPlayer Server Functions
#'
#' @noRd
mod_PlayersByPlayer_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
