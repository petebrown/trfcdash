#' SeasonTracker_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SeasonTracker_sidebar_ui <- function(id){
  ns <- NS(id)
  # Input for selecting season(s) for analysis
  tagList(
    selectInput(
      inputId = ns("selected_seasons"),
      label = h6("Select seasons:"),
      choices = get_season_list(),
      selected = "2022/23",
      multiple = TRUE
    ),
    hr(),
    # Input for specifying number of fixtures to be listed in results panel
    sliderInput(
      inputId = ns("n_fixtures"),
      label = h6("No. of results/page:"),
      min = 1,
      max = 60,
      value = 10,
      ticks = FALSE,
      step = NULL
    )
  )
}

#' SeasonTracker_sidebar Server Functions
#'
#' @noRd
mod_SeasonTracker_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ssn_tracker_inputs <- list(
      reactive({input$selected_seasons}),
      reactive({input$n_fixtures})
    )
    return(ssn_tracker_inputs)
  })
}
