#' ManagersByManager_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ManagersByManager_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("manager_name"),
      label = h6("Select manager:"),
      choices = sort(unique(results_dataset$manager)),
      selected = "Ian Dawes",
      multiple = FALSE,
      selectize = TRUE
    )
  )
}

#' ManagersByManager_sidebar Server Functions
#'
#' @noRd
mod_ManagersByManager_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    manager_inputs <- list(
      reactive({input$manager_name})
    )
    return(manager_inputs)

  })
}
