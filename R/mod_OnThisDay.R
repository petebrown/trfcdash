#' OnThisDay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OnThisDay_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      fluidPage(

      )
    )
  )
}

#' OnThisDay Server Functions
#'
#' @noRd
mod_OnThisDay_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
