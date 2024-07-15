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

      # UI: Season progress charts ----
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
          # UI: Point accumulation plot ----
          bslib::card(
            class = "borderless",
            full_screen = TRUE,
            bslib::card_title(
              "Point accumulation"
            ),
            plotly::plotlyOutput(ns("pts_plot"))
          ),
          # UI: Points-per-game plot ----
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

      bslib::card(
        uiOutput(ns("dynamicTabs"))
      ),


      # UI: Seasonal league records ----
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Season Record"
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
              inputId = ns("rec_inc_cup_games"),
              label = "Include cup games?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            ),
            hr(),
            radioButtons(
              inputId = ns("rec_pens_as_draw"),
              label = "Treat one-off cup games decided by penalty shoot-out as draws?",
              choices = c("Yes", "No"),
              selected = "Yes",
              inline = TRUE
            ),
          ),
          bslib::card(
            class = "borderless",
            bslib::card_title(
              "Overall Record"
            ),
            bslib::card_body(
              fillable = FALSE,
              reactable::reactableOutput(ns("season_records"))
            )
          ),
          hr(style = "width:30%; margin: 1.5rem auto;"),
          bslib::layout_column_wrap(
            width = "715px",
            bslib::card(
              class = "no-growth",
              bslib::card_title(
                "Home Record"
              ),
              bslib::card_body(
                fillable = FALSE,
                reactable::reactableOutput(ns("season_records_home"))
              )
            ),
            bslib::card(
              class = "no-growth",
              bslib::card_title(
                "Away Record"
              ),
              bslib::card_body(
                fillable = FALSE,
                reactable::reactableOutput(ns("season_records_away"))
              )
            )
          )
        )
      ),


      # UI: Player appearance heatmap reactable ----
      bslib::card(
        bslib::card_header(
          class = "bg-dark d-flex justify-content-between",
          "Player appearances",
          # Heatmap options ----
          popover_options(ns)
        ),
        bslib::card_body(
          uiOutput(ns("app_reactable")),
          div(
            class = "d-flex justify-content-center",
            style = "font-size: small; color: black;",
            div(
              style = "margin: 0 1rem; background: #f7f7f7; padding: 0.5rem 0.5rem 0; border: #C3CCD9; border-style: solid; border-width: thin;",
              checkboxGroupInput(
                inputId = ns("summary_stats"),
                label = tags$b("Summary stats: "),
                choiceNames = c(
                  "Appearances",
                  "Goals",
                  "Minutes played",
                  "Cards",
                  "Win %",
                  "Games per goal",
                  "Points per game (League)"
                ),
                choiceValues = c(
                  "apps",
                  "goals",
                  "mins_played",
                  "cards",
                  "win_pc",
                  "games_per_goal",
                  "ppg"
                ),
                inline = TRUE
              )
            )
          ),
          p(
            style = "text-align: right; color: grey; font-size: small",
            "Games per goal based on total minutes played. Win percentage based on games started."
          )
        )
      ),


      # UI: Player appearance table ----
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


      # UI: Streaks ----
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


      # UI: Results, line-ups and league tables ----
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


      # UI: Goalscorer charts ----
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

    base_df <- reactive({
      base_season_tracker_df(selected_seasons())
    })

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
      rec_inc_cup_games <- reactive({
        input$rec_inc_cup_games
      })

      rec_pens_as_draw <- reactive({
        input$rec_pens_as_draw
      })

      reactable::renderReactable(
        ssn_recs_reactable(selected_seasons(), venues, rec_inc_cup_games(), rec_pens_as_draw())
      )
    }

    # CARD 2A: Overall league records for selected seasons
    output$season_records <- render_ssn_records(venues = c("H", "A", "N"))
    # CARD 2B: Home league records for selected seasons
    output$season_records_home <- render_ssn_records(venues = "H")
    # CARD 2C: Away league records for selected seasons
    output$season_records_away <- render_ssn_records(venues = "A")


    ###############################
    # CARD 3: Appearance heat map #
    ###############################

    output$app_reactable <- renderUI({
      req(selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- sort(selected_seasons(), decreasing = FALSE)

        apps2_inc_cup_games <- reactive({
          input$app_react_inc_cup_games
        })
        apps2_pens_as_draw <- reactive({
          input$app_react_pens_as_draw
        })
        apps2_min_starts <- reactive({
          input$app_react_min_starts
        })
        summary_stats <- reactive({
          input$summary_stats
        })
        selected_stat <- reactive({
          input$selected_stat
        })
        player_roles <- reactive({
          input$player_roles
        })


        # Create a tab panel of appearance tables for each  season
        app_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            bslib::card(
              full_screen = TRUE,
              class = c("borderless", "no_padding"),
              style = "font-size: small",
              bslib::card_title(
                paste0("Minutes played, goals and cards in ", season)
              ),
              reactable::renderReactable(
                heatmap_reactable(season, apps2_inc_cup_games(), apps2_pens_as_draw(), apps2_min_starts(), summary_stats(), selected_stat(), player_roles())
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


    output$app_table <- renderUI({
      req(selected_seasons())
      if (!is.null(selected_seasons())) {

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


        # Create a tab panel of appearance tables for each  season
        app_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            bslib::card(
              full_screen = TRUE,
              class = c("borderless", "no_padding"),
              style = "font-size: small;",
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

    # CARD 5: Output tabbed results for selected seasons
    output$ssn_results <- renderUI({
      req(selected_seasons())
      if (!is.null(selected_seasons())) {
        # Sort selected seasons
        selected_seasons <- isolate(sort(selected_seasons(), decreasing = FALSE))

        inc_cup_games <- input$res_inc_cup_games

        # Create a tab panel of results for eachseason
        ssn_tabs <- lapply(selected_seasons, function(season) {
          tabPanel(
            title = season,
            reactable::renderReactable({
              results_with_subtable(
                df=filter_ssn_results(season),
                inc_cup_games=inc_cup_games,
                show_details='Yes',
                show_imgs='Yes')
            })
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

  })
}
