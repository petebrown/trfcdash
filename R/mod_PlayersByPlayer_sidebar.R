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
      choices = NULL,
      selected = NULL,
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

    updateSelectizeInput(session, 'player_name', choices = sort(unique(player_apps$menu_name)), server = TRUE)

    player_inputs <- list(
      reactive({input$player_name})
    )
    return(player_inputs)

  })
}
