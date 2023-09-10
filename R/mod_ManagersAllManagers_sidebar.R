#' ManagersAllManagers_sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ManagersAllManagers_sidebar_ui <- function(id){
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
    ),

    hr(),

    checkboxGroupInput(
      inputId = ns("league_tiers"),
      label = h6("League tiers:"),
      choices = list(
        "2: Championship" = 2,
        "3: League One" = 3,
        "4: League Two" = 4,
        "5: National League" = 5
      ),
      selected = c(2, 3, 4, 5)
    ),

    radioButtons(
      inputId = ns("includePlayOffs"),
      label = "Include play-off games?",
      choices = c("Yes", "No"),
      selected = "Yes",
      inline = TRUE
    ),

    actionButton(
      inputId = ns("select_all_leagues"),
      label = "All leagues"
    ),
    actionButton(
      inputId = ns("deselect_leagues"),
      label = "Clear"
    ),

    hr(),

    checkboxGroupInput(
      inputId = ns("cup_comps"),
      label = h6("Cup competitions:"),
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
      inputId = ns("select_all_cups"),
      label = "All cups"
    ),
    actionButton(
      inputId = ns("deselect_cups"),
      label = "Clear"
    ),

    hr(),

    checkboxGroupInput(
      inputId = ns("venue_options"),
      label = h6("Venues:"),
      choices = list(
        "Home" = "H",
        "Away" = "A",
        "Neutral" = "N"
      ),
      selected = c("H", "A", "N")),

    hr(),

    sliderInput(
      inputId = ns("min_games"),
      label = h6("Minimum no. of games managed:"),
      min = 1,
      max = 100,
      value = 10,
      sep = "",
      ticks = FALSE,
      step = 1
    ),

    hr(),

    actionButton(
      inputId = ns("reset_input"),
      label = "Reset all"
    )
  )
}

#' ManagersAllManagers_sidebar Server Functions
#'
#' @noRd
mod_ManagersAllManagers_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$select_all_leagues, {
      updateNumericInput(
        inputId = "league_tiers",
        value = c(2, 3, 4, 5)
      )
    })

    observeEvent(input$deselect_leagues, {
      updateNumericInput(
        inputId = "league_tiers",
        value = c(0, 0, 0, 0)
      )
    })

    observeEvent(input$select_all_cups, {
      updateTextInput(
      inputId = "cup_comps",
      value = c(
        "Anglo-Italian Cup",
        "Associate Members\' Cup",
        "FA Cup",
        "FA Trophy",
        "Full Members\' Cup",
        "League Cup",
        "War League")
      )
    })

    observeEvent(input$deselect_cups, {
      updateTextInput(
        inputId = "cup_comps",
        value = c("NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL"))
    })

    observeEvent(input$reset_input, {
      updateNumericInput(
        inputId = "year_range",
        value = c(
          min(results_dataset$ssn_year),
          max(results_dataset$ssn_year)
        )
      )

      updateNumericInput(
        inputId = "league_tiers",
        value = c(2, 3, 4, 5)
      )

      updateRadioButtons(
        inputId = "includePlayOffs",
        selected = "Yes"
      )

      updateTextInput(
        inputId = "cup_comps",
        value = c(
          "Anglo-Italian Cup",
          "Associate Members\' Cup",
          "FA Cup",
          "FA Trophy",
          "Full Members\' Cup",
          "League Cup",
          "War League"
        )
      )

      updateTextInput(
        inputId = "venue_options",
        value = c("H", "A", "N"))

      updateNumericInput(
        inputId = "min_games",
        value = 10
      )
    })

    managers_all_inputs <- list(
      reactive({input$year_range}),
      reactive({input$league_tiers}),
      reactive({input$includePlayOffs}),
      reactive({input$cup_comps}),
      reactive({input$venue_options}),
      reactive({input$min_games})
    )
    return(managers_all_inputs)

  })
}
