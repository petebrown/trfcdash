#' OnThisDay_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OnThisDay_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    dateInput(
      inputId = ns("otd_date"),
      label = "Select date:",
      min = min(player_info$player_dob, na.rm = TRUE),
      # max = lubridate::now("GMT"),
      format = "dd/mm/yyyy",
      startview = "month",
      # language = "en-GB",
      autoclose = TRUE,
      weekstart = 1
    ),

    radioButtons(
      inputId = ns("otd_inc_year"),
      label = "Filter to specific year?",
      choices = c("Yes", "No"),
      selected = "No",
      inline = TRUE
    )
  )
}

#' OnThisDay_sidebar Server Functions
#'
#' @noRd
mod_OnThisDay_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

      OnThisDay_inputs <- list(
        reactive({input$otd_date}),
        reactive({input$otd_inc_year})
      )
      return(OnThisDay_inputs)

  })
}
