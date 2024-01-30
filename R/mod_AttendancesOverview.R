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
    plotly::plotlyOutput(ns("plot_av_atts"))
  )
}

#' AttendancesOverview Server Functions
#'
#' @noRd
mod_AttendancesOverview_server <- function(id, year_range){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_av_atts <- plotly::renderPlotly(
      plot_av_attendances(year_range())
    )

  })
}
