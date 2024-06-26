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
  # Input for selecting season(s) for analysis ----
  tagList(
    selectInput(
      inputId = ns("selected_seasons"),
      label = h6("Select seasons:"),
      choices = get_season_list(),
      selected = max(get_season_list()),
      multiple = TRUE
    ),

    hr(),

    # Check boxes for selecting league tiers ----
    checkboxGroupInput(
      inputId = ns("control_tiers"),
      label = h6("Filter seasons by league tier:"),
      choices = list(
        "2: Championship" = 2,
        "3: League One" = 3,
        "4: League Two" = 4,
        "5: National League" = 5
      ),
      selected = c(2, 3, 4, 5),
      inline = TRUE,
      width = "100%"
    )
  )
}

#' SeasonTracker_sidebar Server Functions
#'
#' @noRd
mod_SeasonTracker_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observe({
      selected_tiers <- input$control_tiers

      season_list <- get_div_list_by_tier(selected_tiers)

      updateSelectInput(
        session = session,
        inputId = "selected_seasons",
        choices = season_list,
        selected = max(season_list)
      )
    })

    ssn_tracker_inputs <- list(
      reactive({input$selected_seasons})
    )

    return (ssn_tracker_inputs)
  })
}
