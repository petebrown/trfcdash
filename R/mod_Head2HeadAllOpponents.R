#' Head2HeadAllOpponents UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Head2HeadAllOpponents_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Head2Head All Opponents shit goes HERE!")
  )
}

#' Head2HeadAllOpponents Server Functions
#'
#' @noRd
mod_Head2HeadAllOpponents_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
