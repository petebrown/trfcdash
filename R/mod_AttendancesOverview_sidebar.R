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
    sliderInput(
      inputId = ns("year_range"),
      label = h6("Select season range:"),
      min = min(results_dataset$ssn_year),
      max = max(results_dataset$ssn_year),
      sep = "",
      ticks = FALSE,
      step = 1,
      value = c(
        min(results_dataset$ssn_year),
        max(results_dataset$ssn_year)
      )
    )
  )
}

#' AttendancesOverview_sidebar Server Functions
#'
#' @noRd
mod_AttendancesOverview_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    AttendancesOverview_inputs <- list(
      reactive({input$year_range}) #1
    )
    return(AttendancesOverview_inputs)

  })
}
