#' AttendancesBySeason_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_AttendancesBySeason_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::accordion(
      bslib::accordion_panel(
        "Dates",
        icon = bsicons::bs_icon("menu-app")
      )
    )

  )
}

#' AttendancesBySeason_sidebar Server Functions
#'
#' @noRd
mod_AttendancesBySeason_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_AttendancesBySeason_sidebar_ui("AttendancesBySeason_sidebar_1")

## To be copied in the server
# mod_AttendancesBySeason_sidebar_server("AttendancesBySeason_sidebar_1")
