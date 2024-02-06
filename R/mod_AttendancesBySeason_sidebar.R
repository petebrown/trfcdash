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
    opts_select_seasons(ns, "atts_by_season"),
    # selectInput(
    #   inputId = ns("atts_by_season_selected_seasons"),
    #   label = h6("Select seasons:"),
    #   choices = get_season_list(),
    #   selected = max(get_season_list()),
    #   multiple = TRUE
    # ),
    opts_filter_comps(ns, "atts_by_season")
  )
}

#' AttendancesBySeason_sidebar Server Functions
#'
#' @noRd
mod_AttendancesBySeason_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    AttendancesBySeason_inputs <- list(
      reactive({input$atts_by_season_selected_seasons}), #1
      reactive({input$atts_by_season_cup_comps}) #2
    )

    return(AttendancesBySeason_inputs)
  })
}
