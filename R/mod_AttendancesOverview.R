#' AttendancesOverview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_AttendancesOverview_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("AttendancesOverview shit goes HERE!")
  )
}

#' AttendancesOverview Server Functions
#'
#' @noRd
mod_AttendancesOverview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
