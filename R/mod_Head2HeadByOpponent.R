#' Head2HeadByOpponent UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Head2HeadByOpponent_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Head2Head by Opponent shit goes HERE!")
  )
}

#' Head2HeadByOpponent Server Functions
#'
#' @noRd
mod_Head2HeadByOpponent_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
