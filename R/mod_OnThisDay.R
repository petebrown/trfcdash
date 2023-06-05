#' OnThisDay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OnThisDay_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      fluidPage(
        shinydashboardPlus::box(
          width = 12,
          title = "Date Selector",
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            column(
              width = 3,
              selectInput(
                inputId = ns("selected_day"),
                label = "Select day",
                choices = get_day_list(),
                selected = lubridate::day(lubridate::now()),
                multiple = FALSE
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = ns("selected_month"),
                label = "Select month",
                choices = get_month_list(),
                selected = lubridate::month(lubridate::now(), label = TRUE, abbr = FALSE),
                multiple = FALSE
              )
            ),
            column(
              width = 3,
              selectInput(
                inputId = ns("selected_year"),
                label = "Select year (optional)",
                choices = get_year_list(),
                selected = 1,
                multiple = FALSE
              )
            )
          )
        ),
        shinydashboardPlus::box(
          width = 12,
          title = "OTD Matches",
          footer = NULL,
          solidHeader = TRUE,
          status = "success",
          scrollX = TRUE,
          DT::dataTableOutput(ns("otd_results"))
        )
      )
    )
  )
}

#' OnThisDay Server Functions
#'
#' @noRd
mod_OnThisDay_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Return OTD results for selected day and month
    output$otd_results <- DT::renderDT({
      get_otd_results(input$selected_day, input$selected_month)
    },
    rownames = FALSE,
    options = list(pageLength = 5, dom = 'tip', info = FALSE, paging=FALSE))

  })
}
