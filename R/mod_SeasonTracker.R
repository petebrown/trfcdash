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
        bslib::card_header(
          class = "bg-dark",
          "Season Progress"
        ),
        bslib::card_body(
          min_height = "600px",
          bslib::card(
            full_screen = TRUE,
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
            full_screen = TRUE,
            bslib::card_title(
              "Point accumulation"
            ),
            plotly::plotlyOutput(ns("pts_plot"))
          ),
          bslib::card(
            class = "borderless",
            full_screen = TRUE,
            bslib::card_title(
              "Points-per-game"
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
            reactable::reactableOutput(ns("season_records"))
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
              reactable::reactableOutput(ns("season_records_home"))
            )
          ),
          bslib::card(
            bslib::card_title(
              "Away League Record"
            ),
            bslib::card_body(
              fillable = FALSE,
              reactable::reactableOutput(ns("season_records_away"))
            )
          )
        )
      ),


      # Card containing player appearance heatmaps
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Player appearances"
        ),
        bslib::card_body(
          uiOutput(ns("app_heatmaps"))
        )
      ),


      # Card containing player appearance table
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Player appearances"
        ),
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            position = "left",
            width = 200,
            bg = "#4c668d",
            class = "card-sidebar",
            open = FALSE,
            radioButtons(
              inputId = ns("app_tab_inc_cup_games"),
              label = "Include cup games?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            ),
            hr(),
            radioButtons(
              inputId = ns("app_tab_pens_as_draw"),
              label = "Treat one-off cup games decided by penalty shoot-out as draws?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            ),
            hr(),
            sliderInput(
              inputId = ns("app_tab_min_starts"),
              label = "Minimum no. of starts:",
              min = 0,
              max = 50,
              value = 0,
              sep = "",
              ticks = FALSE,
              step = 1
            )
          ),
          uiOutput(ns("app_table")),
          p(
            style = "text-align: right; color: grey; font-size: small",
            "Games per goal based on total minutes played. Win percentage based on games started."
          )
        )
      ),


      # Card containing longest streaks
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Longest Streaks"
        ),
        bslib::layout_sidebar(
          fillable = FALSE,
          sidebar = bslib::sidebar(
            position = "left",
            width = 200,
            bg = "#4c668d",
            class = "card-sidebar",
            open = FALSE,
            radioButtons(
              inputId = ns("streaks_inc_cup_games"),
              label = "Include cup games?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            ),
            hr(),
            radioButtons(
              inputId = ns("streaks_pens_as_draw"),
              label = "Treat one-off cup games decided by penalty shoot-out as draws?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            )
          ),
          reactable::reactableOutput(ns("streaks"))
        )
      ),


      # Card containing season results with drop-down line-ups
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Season Results"
        ),
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            position = "left",
            width = 200,
            bg = "#4c668d",
            class = "card-sidebar",
            open = FALSE,
            radioButtons(
              inputId = ns("res_inc_cup_games"),
              label = "Include cup games?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            )
          ),
          uiOutput(ns("ssn_results"))
        )
      ),


      # Card containing top scorer charts
      bslib::card(
        class = "top-scorers",
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Top Scorers"
        ),
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            position = "left",
            width = 200,
            bg = "#4c668d",
            class = "card-sidebar",
            open = FALSE,
            sliderInput(
              inputId = ns("n_scorers"),
              label = "No. of top scorers:",
              min = 1,
              max = 5,
              value = 4,
              sep = "",
              ticks = FALSE,
              step = 1
            ),
            hr(),
            radioButtons(
              inputId = ns("inc_cup_games"),
              label = "Include cup games?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            ),
          ),
          uiOutput(ns("top_scorers"))
        )
      )
    )
  )
}

#' SeasonTracker Server Functions
#'
#' @noRd
mod_SeasonTracker_server <- function(id, selected_seasons){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


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
      reactable::renderReactable(
        output_ssn_records(selected_seasons(), venues)
      )
    }

    # CARD 2A: Overall league records for selected seasons
    output$season_records <- render_ssn_records(venues = c("H", "A"))
    # CARD 2B: Home league records for selected seasons
    output$season_records_home <- render_ssn_records(venues = "H")
    # CARD 2C: Away league records for selected seasons
    output$season_records_away <- render_ssn_records(venues = "A")


    ###############################
    # CARD 3: Appearance heat map #
    ###############################

    # CARD 3: Output tabbed heatmaps showing player appearances in selected seasons
    output$app_heatmaps <- renderUI({
      req(selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = FALSE)

        # Create a tab panel of appearance heatmaps for each  season
        app_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            bslib::card(
              full_screen = TRUE,
              class = c("borderless", "no_padding"),
              bslib::card_title(
                paste0("Appearances, goals and cards in ", season)
              ),
              min_height = "880px",
              renderPlot({
                output_app_heatmap(season)
              })
            )
          )
        })

        # Return a tabsetPanel containing season results
        do.call(tabsetPanel, app_tabs)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })

    output$app_table <- renderUI({
      req(selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = FALSE)

        apps_inc_cup_games <- reactive({
          input$app_tab_inc_cup_games
        })
        apps_pens_as_draw <- reactive({
          input$app_tab_pens_as_draw
        })
        apps_min_starts <- reactive({
          input$app_tab_min_starts
        })


        # Create a tab panel of appearance heatmaps for each  season
        app_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            bslib::card(
              full_screen = TRUE,
              class = c("borderless", "no_padding"),
              style = "font-size: smaller;",
              bslib::card_title(
                paste0("Appearances, goals and cards in ", season)
              ),
              reactable::renderReactable(
                output_app_table(season, apps_inc_cup_games(), apps_pens_as_draw(), apps_min_starts())
              )
            )
          )
        })

        # Return a tabsetPanel containing season results
        do.call(tabsetPanel, app_tabs)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })


    ###########################
    # CARD 4: LONGEST STREAKS #
    ###########################

    streaks_inc_cup_games <- reactive({
      input$streaks_inc_cup_games
    })
    streaks_pens_as_draw <- reactive({
      input$streaks_pens_as_draw
    })

    # CARD 4: Longest streaks in selected seasons
    output$streaks <- reactable::renderReactable(
      render_streaks(selected_seasons(), streaks_inc_cup_games(), streaks_pens_as_draw())
    )


    ###################
    # CARD 5: RESULTS #
    ###################

    # CARD 5: Output tabbed results for selected seasions
    output$ssn_results <- renderUI({
      req(selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = FALSE)
        inc_cup_games <- input$res_inc_cup_games
        n_fixtures <- input$n_fixtures

        # Create a tab panel of results for each  season
        ssn_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            reactable::renderReactable(
              output_ssn_reactable(season, inc_cup_games)
            )
          )
        })

        # Return a tabsetPanel containing season results
        do.call(tabsetPanel, ssn_tabs)
      } else {
        p("Please select one or more seasons from the dropdown menu.")
      }
    })


    #######################
    # CARD 6: Top Scorers #
    #######################

    output$top_scorers <- renderUI({
      inc_cup_games <- input$inc_cup_games
      n_scorers <- input$n_scorers

      plot_top_scorers(selected_seasons(), inc_cup_games, n_scorers)
    })


    # CARD 5: Output nested cards containing top scorers plots for selected seasons
    # output$boxed_ssn_scorers <- renderUI({
    #   if (!is.null(selected_seasons())) {
    #     # Get selected seasons
    #     selected_seasons <- sort(selected_seasons(), decreasing = FALSE)
    #
    #     n_plots <- length(selected_seasons())
    #     max_goals <- get_max_goals(selected_seasons())
    #
    #     # Create a list of scorer charts - one per season
    #     ssn_scorer_boxes <- lapply(selected_seasons, function(season) {
    #       bslib::card(
    #         class = "borderless",
    #         bslib::card_title(
    #           season
    #         ),
    #         bslib::card_body(
    #           plot_ssn_scorers(season, max_goals, n_plots)
    #         )
    #       )
    #     })
    #     # Return all charts - 100% width for one, 33% width for multiples of
    #     # three, otherwise 50% width
    #     bslib::layout_column_wrap(
    #       width = ifelse(length(selected_seasons) == 1, 1,
    #                      ifelse(length(selected_seasons) %% 3 == 0, 1/3,
    #                             ifelse(length(selected_seasons) %% 2 == 0, 1/2, 1/3))),
    #       !!!ssn_scorer_boxes,
    #       heights_equal = "all", fixed_width = TRUE, fill = TRUE
    #     )
    #   } else {
    #     p("Please select one or more seasons from the dropdown menu.")
    #   }
    # })

  })
}
