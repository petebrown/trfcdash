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
        footer = "This is the footer text",
        solidHeader = TRUE,
        status = "primary",
        fluidRow(
          column(
            width = 2,
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
              inputId = "ns(selected_chart_type)",
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
        footer = "Here is some footer text",
        solidHeader = TRUE,
        status = "success",
        scrollX = TRUE,
        DT::dataTableOutput(ns("season_records"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Streaks",
        footer = "Here is some footer text",
        solidHeader = TRUE,
        status = "info",
        DT::dataTableOutput(ns("streaks"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Results",
        footer = "Here is some footer text",
        solidHeader = TRUE,
        status = "danger",
        uiOutput(ns("ssn_results"))
      ),
      shinydashboardPlus::box(
        width = 12,
        title = "Top Scorers",
        solidHeader = TRUE,
        status = "warning",
        shinydashboardPlus::box(
          title = "2022/23",
          headerBorder = FALSE,
          plotly::plotlyOutput(ns("scorers_plot_1"))
        ),
        shinydashboardPlus::box(
          title = "2021/22",
          headerBorder = FALSE,
          plotly::plotlyOutput(ns("scorers_plot_2"))
        ),
        shinydashboardPlus::box(
          title = "2020/21",
          headerBorder = FALSE,
          plotly::plotlyOutput(ns("scorers_plot_3"))
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

    # Season plot
    output$seasons_plot <- plotly::renderPlotly({
      p <- shinipsum::random_ggplot(type = "line")
      plotly::ggplotly(p)
    })

    # Season Records
    output$season_records <- DT::renderDT({
      shinipsum::random_DT(nrow = 2, ncol = 15, "numeric")
    })

    # Season streaks
    output$streaks <- DT::renderDT({
      get_streaks(input$selected_seasons)
    })

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

    # Scorers plots
    output$scorers_plot_1 <- plotly::renderPlotly({
      p <- shinipsum::random_ggplot(type = "bar") + ggplot2::coord_flip()
      plotly::ggplotly(p)
    })
    output$scorers_plot_2 <- plotly::renderPlotly({
      p <- shinipsum::random_ggplot(type = "bar") + ggplot2::coord_flip()
      plotly::ggplotly(p)
    })
    output$scorers_plot_3 <- plotly::renderPlotly({
      p <- shinipsum::random_ggplot(type = "bar") + ggplot2::coord_flip()
      plotly::ggplotly(p)
    })

  })
}
