#' Head2HeadAllOpponents_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Head2HeadAllOpponents_sidebar_ui <- function(id){
  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("year_range"),
      label = h5("Select season range:"),
      min = 1921, # min(results_dataset$season),
      max = 2022, # max(results_dataset$season),
      sep = "",
      ticks = FALSE,
      step = 1,
      value = c(1921, 2022) # c(min(results_dataset$season), max(results_dataset$season))
    ),

    checkboxGroupInput(
      inputId = ns("leagueTiers"),
      label = h5("League tiers:"),
      choices = list(
        "2: Championship" = 2,
        "3: League One" = 3,
        "4: League Two" = 4,
        "5: National League" = 5
      ),
      selected = c(2, 3, 4, 5)
    ),

    actionButton(
      inputId = ns("selectAllLeagues"),
      label = "Select all league tiers"
    ),
    actionButton(
      inputId = ns("deselectLeagues"),
      label = "Deselect all league tiers"
    ),

    checkboxGroupInput(
      inputId = ns("cupComps"),
      label = h5("Cup competitions:"),
      choices = list(
        "Anglo-Italian Cup" = "Anglo-Italian Cup",
        "Associate Members' Cup" = "Associate Members\' Cup",
        "FA Cup" = "FA Cup",
        "FA Trophy" = "FA Trophy",
        "Full Members' Cup" = "Full Members\' Cup",
        "League Cup" = "League Cup",
        "War League" = "War League"
      ),
      selected = c(
        "Anglo-Italian Cup",
        "Associate Members\' Cup",
        "FA Cup",
        "FA Trophy",
        "Full Members\' Cup",
        "League Cup",
        "War League"
      )
    ),

    actionButton(
      inputId = ns("selectAllCups"),
      label = "Select all cups"
    ),
    actionButton(
      inputId = ns("deselectCups"),
      label = "Deselect all cups"
    ),

    checkboxGroupInput(
      inputId = ns("venueOptions"),
      label = h5("Venues:"),
      choices = list(
        "Home" = "H",
        "Away" = "A",
        "Neutral" = "N"
      ),
      selected = c("H", "A", "N")),

    sliderInput(
      inputId = ns("minGames"),
      label = h5("Minimum no. of meetings:"),
      min = 1,
      max = 100,
      value = 10,
      sep = "",
      ticks = FALSE,
      step = 1
    ),

    actionButton(
      inputId = ns("reset_input"),
      label = "Reset inputs"
    )
  )
}

#' Head2HeadAllOpponents_sidebar Server Functions
#'
#' @noRd
mod_Head2HeadAllOpponents_sidebar_server <- function(id, year_range){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    head2head_all_inputs <- list(
      reactive({input$year_range})
    )
    return(head2head_all_inputs)

  })
}

## To be copied in the UI
# mod_Head2HeadAllOpponents_sidebar_ui("Head2HeadAllOpponents_sidebar_1")

## To be copied in the server
# mod_Head2HeadAllOpponents_sidebar_server("Head2HeadAllOpponents_sidebar_1")
