#' AttendancesOverview_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_AttendancesOverview_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    opts_filter_years(ns, "atts_overview"),

    hr(),


    opts_filter_comps(ns, "atts_overview")
  )
}

#' AttendancesOverview_sidebar Server Functions
#'
#' @noRd
mod_AttendancesOverview_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    AttendancesOverview_inputs <- list(
      reactive({input$atts_overview_year_range}), #1
      reactive({input$atts_overview_cup_comps}) #2
    )
    return(AttendancesOverview_inputs)

  })
}
