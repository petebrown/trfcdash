#' SeasonTracker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SeasonTracker_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        selectInput(
          inputId = "selected_seasons",
          label = "Select seasons:",
          choices = get_season_list(),
          selected = "2022/23",
          multiple = TRUE
        )
      ),
      fluidRow(
        radioButtons(
          inputId = "selected_chart_type",
          label = "Choose chart type:",
          choices = get_chart_options(),
          selected = "League Position",
          inline = TRUE
        )
      )
    )
  )
}

#' SeasonTracker Server Functions
#'
#' @noRd
mod_SeasonTracker_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
