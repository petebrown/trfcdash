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
    atts_by_season <- plotOutput(ns("atts_by_season"))
  )
}

#' AttendancesBySeason Server Functions
#'
#' @noRd
mod_AttendancesBySeason_server <- function(id, selected_seasons, cup_comps){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$atts_by_season <- renderPlot(
      plot_season_atts(c(selected_seasons()), cup_comps())
    )

  })
}
