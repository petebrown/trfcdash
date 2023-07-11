#' PlayersAllPlayers_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PlayersAllPlayers_sidebar_ui <- function(id){
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

#' PlayersAllPlayers_sidebar Server Functions
#'
#' @noRd
mod_PlayersAllPlayers_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    players_all_inputs <- list(
      reactive({input$year_range})
    )
    return(players_all_inputs)

  })
}

## To be copied in the UI
# mod_PlayersAllPlayers_sidebar_ui("PlayersAllPlayers_sidebar_1")

## To be copied in the server
# mod_PlayersAllPlayers_sidebar_server("PlayersAllPlayers_sidebar_1")
