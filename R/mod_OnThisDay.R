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
              width = 4,
              selectInput(
                inputId = ns("selected_day"),
                label = "Select day",
                choices = c(1:31),
                selected = 1,
                multiple = FALSE
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = ns("selected_nonth"),
                label = "Select month",
                choices = lubridate::month(c(1:12), label = TRUE, abbr = FALSE),
                selected = "January"
              )
            ),
            column(
              width = 4,
              selectInput(
                inputId = ns("selected_year"),
                label = "Select year (optional)",
                choices = c(min_year:max_year),
                selected = 1,
                multiple = FALSE
              )
            )
          )
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

  })
}
