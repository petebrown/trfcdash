# User inputs to be displayed in Season Tracker sidebar
SeasonTracker_sidebar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("selected_seasons"),
      label = "Select seasons:",
      choices = get_season_list(),
      selected = "2022/23",
      multiple = TRUE
    ),
    hr(),
    sliderInput(ns("n_fixtures"), "No. of results/page:", min = 1, max = 60, value = 10, ticks = FALSE, step = NULL)
  )
}

# Create Season Tracker sidebar using inputs from SeasonTracker_sidebar_ui
SeasonTracker_sidebar <- function() {
  bslib::page_fluid(
    SeasonTracker_sidebar_ui("SeasonTracker_sidebar_ui_1")
  )
}

# Season Tracker sidebar for sending user inputs to main app_server
SeasonTracker_sidebar_server <- function(id) {
  moduleServer( id, function(input, output, session) {

    ssn_tracker_inputs <- list(
      reactive({input$selected_seasons}),
      reactive({input$n_fixtures})
    )
    return(ssn_tracker_inputs)
  })
}

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
    bslib::page_fluid(
      # Card containing main season progress chart
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Season Progress"
        ),
        bslib::card_body(
          min_height = "600px",
          bslib::card(
            class = "borderless",
            bslib::card_title(
              "League Positions"
            ),
            plotly::plotlyOutput(ns("seasons_plot"))
          ),
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            class = "borderless",
            bslib::card_title(
              "Point accumulation"
            ),
            plotly::plotlyOutput(ns("pts_plot"))
          ),
          bslib::card(
            class = "borderless",
            bslib::card_title(
              "Average points-per-game"
            ),
            plotly::plotlyOutput(ns("ppg_plot"))
          )
        ),
        uiOutput(ns("footer_text"))
      ),

      # Card containing each season's league record
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Season Record"
        ),
        bslib::card(
          class = "borderless",
          bslib::card_title(
            "Overall League Record"
          ),
          bslib::card_body(
            fillable = FALSE,
            DT::dataTableOutput(ns("season_records"))
          )
        ),
        hr(style = "width:30%; margin: 1.5rem auto;"),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_title(
              "Home League Record"
            ),
            bslib::card_body(
              fillable = FALSE,
              DT::dataTableOutput(ns("season_records_home"))
            )
          ),
          bslib::card(
            bslib::card_title(
              "Away League Record"
            ),
            bslib::card_body(
              fillable = FALSE,
              DT::dataTableOutput(ns("season_records_away"))
            )
          )
        )
      ),



      # Card containing longest streaks
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Longest Streaks"
        ),
        bslib::card_body(
          fillable = FALSE,
          DT::dataTableOutput(ns("streaks"))
        )
      ),

      # Card containing each season's results
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Results"
        ),
        uiOutput(ns("ssn_results"))
      ),

      # Card containing top scorer charts
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Top Scorers"
        ),
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
mod_SeasonTracker_server <- function(id, selected_seasons, n_fixtures){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #
    # Function to produce main charts
    #
    render_plot <- function(chart_type) {
      plotly::renderPlotly({
        output_seasons_plot(c(selected_seasons()), chart_type)
      })
    }

    output$seasons_plot <- render_plot("league_pos")
    output$pts_plot <- render_plot("pts")
    output$ppg_plot <- render_plot("ppg")

    # Add note about COVID season if 2019/20 in selected seasons
    output$footer_text <- renderText({
      if ("2019/20" %in% selected_seasons()) {
        "N.B. 2019/20 season ended after 34 games due to COVID-19."
      } else {
        ""
      }
    })

    #
    # Season Records
    # Function to produce season records
    #
    render_ssn_records <- function(venues) {
      DT::renderDT(
        output_ssn_records(selected_seasons(), venues),
        rownames = FALSE,
        options = list(
          pageLength = 5,
          dom = 'tip',
          info = FALSE,
          paging = FALSE,
          fillContainer = TRUE
        )
      )
    }

    # Output seasons records HOME AND AWAY
    output$season_records <- render_ssn_records(venues = c("H", "A"))
    # Output season records HOME
    output$season_records_home <- render_ssn_records(venues = "H")
    # Output season records AWAY
    output$season_records_away <- render_ssn_records(venues = "A")

    #
    # Output longest streaks in selected seasons
    #
    output$streaks <- DT::renderDT(
      get_streaks(selected_seasons()),
      rownames = FALSE,
      options = list(pageLength = 5, dom = 'tip', info = FALSE, paging=FALSE, fillContainer = TRUE)
    )

    #
    # Full results table
    #
    output$ssn_results <- renderUI({
      req(n_fixtures(), selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = TRUE)

        # Create a tab panel of results for each  season
        ssn_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            output_ssn_results(season, n_fixtures())
          )
        })

        # Return a tabsetPanel containing season results
        do.call(tabsetPanel, ssn_tabs)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })

    # Individual top scorer charts
    output$boxed_ssn_scorers <- renderUI({
      if (!is.null(selected_seasons())) {
        # Get selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = TRUE)

        n_plots <- length(selected_seasons())
        max_goals <- get_max_goals(selected_seasons())

        # Create a list of scorer charts - one per season
        ssn_scorer_boxes <- lapply(selected_seasons, function(season) {
          bslib::card(
            class = "borderless",
            bslib::card_title(
              season
            ),
            bslib::card_body(
              plot_ssn_scorers(season, max_goals, n_plots)
            )
          )
        })
        # Return all charts - 100% width for one, 50% width for multiples of two, otherwise 33% width
        bslib::layout_column_wrap(
          width = ifelse(length(selected_seasons) == 1, 1,
                         ifelse(length(selected_seasons) %% 3 == 0, 1/3,
                                ifelse(length(selected_seasons) %% 2 == 0, 1/2, 1/3))),
          !!!ssn_scorer_boxes,
          heights_equal = "all", fixed_width = TRUE, fill = TRUE
        )
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })
  })
}
