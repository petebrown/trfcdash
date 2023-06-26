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
      tags$style(HTML(
        "
        .html-fill-container > .html-fill-item.datatables {
          flex-basis: content;
        }
        "
      )),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Season Progress"
        ),

        bslib::card_body(
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
          plotly::plotlyOutput(ns("seasons_plot"), height = 500)
        ),
        bslib::card_footer(
          uiOutput(ns("footer_text"))
        )
      ),



      # Card containing each season's league record
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Season Record"
        ),
        bslib::card_body(
          DT::dataTableOutput(ns("season_records"))
        )
      ),
      # Card containing longest streaks
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Longest Streaks"
        ),
        DT::dataTableOutput(ns("streaks"))
      ),
      # Card containing each season's results
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Results"
        ),
        uiOutput(ns("ssn_results"))
      ),
      # Card containing top scorer charts
      bslib::card(
        bslib::card_header("Top Scorers"),
        bslib::card_body(
          uiOutput(ns("boxed_ssn_scorers"))
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
      output_seasons_plot(input$selected_seasons, input$selected_chart_type)
    })
    observeEvent(input$selected_seasons, {
      if ("2019/20" %in% input$selected_seasons) {
        output$footer_text <- renderText("2019/20 ended early due to COVID-19.")
      } else {
        output$footer_text <- renderText("")
      }
    })



















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

    # Individual top scorer charts
    output$boxed_ssn_scorers <- renderUI({
      if (!is.null(input$selected_seasons)) {
        # Get selected seasons
        selected_seasons <- sort(input$selected_seasons, decreasing = TRUE)

        max_goals <- get_max_goals(input$selected_seasons)

        # Create a list of scorer charts
        ssn_scorer_boxes <- lapply(selected_seasons, function(season) {
          bslib::card(
            bslib::card_header(
              season
            ),
            bslib::card_body(
              plot_ssn_scorers(season, max_goals)
            )
          )
        })
        # Return a tagList containing all top scorer plots
        bslib::layout_column_wrap(
          width = ifelse(length(selected_seasons) == 1, 1,
                         ifelse(length(selected_seasons)%%2 == 0, 1/2, 1/3)),
          !!!ssn_scorer_boxes,
          heights_equal = "all", fixed_width = TRUE, fill = TRUE
        )
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })

    # TabPanel containing top scorer cbarts
    # output$ssn_scorers <- renderUI({
    #   if (!is.null(input$selected_seasons)) {
    #     # Get selected seasons
    #     selected_seasons <- sort(input$selected_seasons, decreasing = TRUE)
    #
    #     # Create a tab panel for each selected season
    #     ssn_scorer_tabs <- lapply(selected_seasons, function(season) {
    #       tabPanel(
    #         title = season,
    #         plot_ssn_scorers(season, max_goals)
    #       )
    #     })
    #
    #     # Return the tabsetPanel containing season results
    #     do.call(tabsetPanel, ssn_scorer_tabs)
    #   } else {
    #     p("Please select one or more seasons from the dropdown menu.")
    #   }
    # })
  })
}
