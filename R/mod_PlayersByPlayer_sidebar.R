#' PlayersByPlayer_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PlayersByPlayer_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("player_name"),
      label = h6("Select player:"),
      choices = sort(unique(player_apps$player_name)),
      selected = NULL,
      # selected = sort(unique(player_apps$player_name))[[1]],
      multiple = FALSE,
      selectize = TRUE
    )
  )
}

#' PlayersByPlayer_sidebar Server Functions
#'
#' @noRd
mod_PlayersByPlayer_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    player_inputs <- list(
      reactive({input$player_name})
    )
    return(player_inputs)

  })
}
