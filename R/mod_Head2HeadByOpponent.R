#' Head2HeadByOpponent UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Head2HeadByOpponent_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(
      class="row",
      style="margin-bottom: 2rem;",
      div(
        class="col-sm-auto d-flex align-items-center",
        style="text-align: left; margin-left: 1rem",
        h1(textOutput(ns("opponent")), class = "display-1 opponent-title")
      ),
      div(
        class="col align-items-center",
        style="margin-right: 1rem;",
        uiOutput(ns("club_crest"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Overall Record"
      ),
      bslib::card_body(
        plotOutput(ns("h2h_plot")),
        reactable::reactableOutput(ns("h2h_record"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Records by Venue"
      ),
      bslib::card_body(
        plotOutput(ns("h2h_venue_plot")),
        reactable::reactableOutput(ns("h2h_venue_record"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Streaks"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("h2h_streaks"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Managerial Records"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("h2h_man_records"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Previous Meetings"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("h2h_meetings"))
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark d-flex justify-content-between",
        "Player Stats"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("plr_stats_vs_opp"), height = "auto"),
        p(
          style = "text-align: right; color: grey; font-size: small",
          "Games per goal based on total minutes played. Win percentage based on games started."
        )
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark d-flex justify-content-between",
        "Biggest Wins",
        wins_popover_options(ns("wins_by_opp_react_min_diff"), min_goals=1)
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("h2h_biggest_wins"), height = "auto"),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark d-flex justify-content-between",
        "Biggest Defeats",
        defeats_popover_options(ns("defeats_by_opp_react_min_diff"), min_goals=1)
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("h2h_biggest_defeats"), height = "auto"),
      )
    )

  )
}

#' Head2HeadByOpponent Server Functions
#'
#' @noRd
mod_Head2HeadByOpponent_server <- function(id, opponent, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$club_crest <- renderUI({
      file_path <- map_club_to_crest(opponent())

      img(
        src = file_path,
        height = 200,
        alt = opponent(),
        class = "float-right"
      )
    })

    output$opponent <- renderText({
      opponent()
    })

    base_df <- reactive({
      base_h2hByOpponenet_df(
        opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
      )
    })

    output$h2h_plot <- {
      renderPlot(
        plot_h2h_summary(
          base_df()
        ),
        bg = "transparent"
      )
    }

    output$h2h_record <- {
      reactable::renderReactable(
        reactable::reactable(
          get_h2h_summary(
            base_df()
          ),
          class = "apps-reactable",
          style = list(
            fontSize = "0.9rem",
            fontWeight = 300
          ),
          defaultColDef = reactable::colDef(
            align = "center",
          ),
          columns = list(
            GD = reactable::colDef(
              vAlign = "center",
              minWidth = 70,
              # Function to add plus sign (+) before positive figures
              cell = function(value) {
                format_gd(value)
              }
            )          )
        )
      )
    }

    output$h2h_venue_plot <- {
      renderPlot(
        plot_h2h_by_venue(
          base_df()
        ),
        bg = "transparent"
      )
    }

    output$h2h_venue_record <- {
      reactable::renderReactable(
        reactable::reactable(
          get_h2h_by_venue(
            base_df()
          ),
          class = "apps-reactable",
          style = list(
            fontSize = "0.9rem",
            fontWeight = 300
          ),
          columns = list(
            venue = reactable::colDef(
              name = ""
            ),
            GD = reactable::colDef(
              vAlign = "center",
              minWidth = 70,
              # Function to add plus sign (+) before positive figures
              cell = function(value) {
                format_gd(value)
              }
            )
          )
        )
      )
    }

    output$h2h_streaks <- {
      reactable::renderReactable(
        get_streaks_by_team(
          opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
        )
      )
    }

    output$h2h_man_records <- {
      reactable::renderReactable(
        mgr_img_table(
          get_h2h_man_summary(
            base_df()
          )
        )
      )
    }

    output$h2h_meetings <- {
      reactable::renderReactable(
        get_h2h_meetings(
          base_df()
        )
      )
    }

    output$plr_stats_vs_opp <- {
      reactable::renderReactable(
        get_h2h_player_stats(
          base_df()
        )
      )
    }

    min_win_diff <- reactive({
      input$wins_by_opp_react_min_diff
    })

    output$h2h_biggest_wins <- {
      reactable::renderReactable(
        get_biggest_wins_by_opp(
          base_df(), min_win_diff()
        )
      )
    }

    min_defeat_diff <- reactive({
      input$defeats_by_opp_react_min_diff
    })

    output$h2h_biggest_defeats <- {
      reactable::renderReactable(
        get_biggest_defeats_by_opp(
          base_df(), min_defeat_diff()
        )
      )
    }

  })
}
