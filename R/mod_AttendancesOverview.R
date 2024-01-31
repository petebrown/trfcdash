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
    plotly::plotlyOutput(ns("plot_av_atts")),

    reactable::reactableOutput(ns("top_atts_by_ssn"))
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

    output$top_atts_by_ssn <- reactable::renderReactable(
      top_n_attendances(
        year_range(),
        n = 1,
        comps = c("League", "FA Cup", "League Cup"),
        venues = "H"
      )
    )

  })
}
