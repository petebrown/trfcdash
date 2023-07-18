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
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Season Reactable"
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("ssn_reactable"))
        )
      ),
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

    output$ssn_reactable <- reactable::renderReactable(
      output_ssn_reactable(c(selected_seasons()))
    )

    ###########################
    # CARD 1: SEASON PROGRESS #
    ###########################

    # Function to produce main charts
    render_plot <- function(chart_type) {
      plotly::renderPlotly({
        output_seasons_plot(c(selected_seasons()), chart_type)
      })
    }

    # CARD 1A: League Positions plot
    output$seasons_plot <- render_plot("league_pos")
    # CARD 1B: Point Accumulation plot
    output$pts_plot <- render_plot("pts")
    # CARD 1C: PPG plot
    output$ppg_plot <- render_plot("ppg")

    # Add note about COVID season if 2019/20 in selected seasons
    output$footer_text <- renderText({
      if ("2019/20" %in% selected_seasons()) {
        "N.B. 2019/20 season ended after 34 games due to COVID-19."
      } else {
        ""
      }
    })

    #########################
    # CARD 2: SEASON RECORD #
    #########################

    # Function to produce season record data tables
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

    # CARD 2A: Overall league records for selected seasons
    output$season_records <- render_ssn_records(venues = c("H", "A"))
    # CARD 2B: Home league records for selected seasons
    output$season_records_home <- render_ssn_records(venues = "H")
    # CARD 2C: Away league records for selected seasons
    output$season_records_away <- render_ssn_records(venues = "A")

    ###########################
    # CARD 3: LONGEST STREAKS #
    ###########################

    # CARD 3: Longest streaks in selected seasons
    output$streaks <- DT::renderDT(
      get_streaks(selected_seasons()),
      rownames = FALSE,
      options = list(pageLength = 5, dom = 'tip', info = FALSE, paging = FALSE, fillContainer = TRUE)
    )

    ###################
    # CARD 4: RESULTS #
    ###################

    # CARD 4: Output tabbed results for selected seasions
    output$ssn_results <- renderUI({
      req(n_fixtures(), selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = FALSE)

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

    #######################
    # CARD 5: Top Scorers #
    #######################

    # CARD 5: Output nested cards containing top scorers plots for selected seasons
    output$boxed_ssn_scorers <- renderUI({
      if (!is.null(selected_seasons())) {
        # Get selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = FALSE)

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
        # Return all charts - 100% width for one, 33% width for multiples of
        # three, otherwise 50% width
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
