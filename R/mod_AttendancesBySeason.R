#' AttendancesBySeason UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_AttendancesBySeason_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Attendances by Season shit goes HERE!")
  )
}

#' AttendancesBySeason Server Functions
#'
#' @noRd
mod_AttendancesBySeason_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
