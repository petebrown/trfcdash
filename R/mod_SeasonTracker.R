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
      shinydashboardPlus::box(
        width = 12,
        title = "Season Progress",
        solidHeader = TRUE,
        status = "primary",
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = ns("selected_seasons"),
              label = "Select seasons:",
              choices = get_season_list(),
              selected = "2022/23",
              multiple = TRUE
            )
          ),
          column(
            width = 3,
            offset = 0,
            selectInput(
              inputId = ns("selected_chart_type"),
              label = "Choose chart type:",
              choices = get_chart_options(),
              selected = "league_pos"
            )
          )
        ),
        plotly::plotlyOutput(ns("seasons_plot"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Season Record",
        footer = NULL,
        solidHeader = TRUE,
        status = "success",
        scrollX = TRUE,
        DT::dataTableOutput(ns("season_records"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Streaks",
        footer = NULL,
        solidHeader = TRUE,
        status = "primary",
        DT::dataTableOutput(ns("streaks"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Results",
        footer = NULL,
        solidHeader = TRUE,
        status = "success",
        uiOutput(ns("ssn_results"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Top Scorers",
        # footer = "Here is some footer text",
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("ssn_scorers"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Top Scorers",
        # footer = "Here is some footer text",
        solidHeader = TRUE,
        status = "primary",
        uiOutput(ns("boxed_ssn_scorers"))
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

    # Season plot
    output$seasons_plot <- plotly::renderPlotly({
      output_seasons_plot(input$selected_seasons, input$selected_chart_type)
    })

    # Season Records
    output$season_records <- DT::renderDT({
      output_ssn_records(input$selected_seasons)
    }, rownames = FALSE,
    options = list(pageLength = 5, dom = 'tip', info = FALSE, paging = FALSE))

    # Season streaks
    output$streaks <- DT::renderDT({
      get_streaks(input$selected_seasons)
    },
    rownames = FALSE,
    options = list(pageLength = 5, dom = 'tip', info = FALSE, paging=FALSE))

    # Full results table
    output$ssn_results <- renderUI({
      if (!is.null(input$selected_seasons)) {
        # Get selected seasons
        selected_seasons <- sort(input$selected_seasons, decreasing = TRUE)

        # Create a tab panel for each selected season
        ssn_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            output_ssn_results(season)
          )
        })

        # Return the tabsetPanel containing season results
        do.call(tabsetPanel, ssn_tabs)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })

    # Full results table
    output$ssn_scorers <- renderUI({
      if (!is.null(input$selected_seasons)) {
        # Get selected seasons
        selected_seasons <- sort(input$selected_seasons, decreasing = TRUE)

        # Create a tab panel for each selected season
        ssn_scorer_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            plot_ssn_scorers(season)
          )
        })

        # Return the tabsetPanel containing season results
        do.call(tabsetPanel, ssn_scorer_tabs)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })

    # Full results table
    output$boxed_ssn_scorers <- renderUI({
      if (!is.null(input$selected_seasons)) {
        # Get selected seasons
        selected_seasons <- sort(input$selected_seasons, decreasing = TRUE)

        # Create a tab panel for each selected season
        ssn_scorer_boxes <- lapply(selected_seasons, function(season) {
          shinydashboardPlus::box(
            width = ifelse(length(selected_seasons) == 1, 12, 6),
            title = season,
            # footer = ifelse(season == "2019/20", "Season ended early", "NULL"),
            headerBorder = FALSE,
            plot_ssn_scorers(season)
          )
        })

        # Return a tagList containing all top scorer plots
        do.call(tagList, ssn_scorer_boxes)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })

  })
}
