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

    h1(textOutput(ns("opponent")), class = "opponent-title"),

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
    )

  )
}

#' Head2HeadByOpponent Server Functions
#'
#' @noRd
mod_Head2HeadByOpponent_server <- function(id, opponent, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$opponent <- renderText({
      opponent()
    })

    output$h2h_plot <- {
      renderPlot(
        plot_h2h_summary(
          opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
        )
      )
    }

    output$h2h_record <- {
      reactable::renderReactable(
        reactable::reactable(
          get_h2h_summary(
            opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
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
          opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
        )
      )
    }

    output$h2h_venue_record <- {
      reactable::renderReactable(
        reactable::reactable(
          get_h2h_by_venue(
            opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
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

    output$h2h_man_records <- {
      reactable::renderReactable(
        mgr_img_table(
          get_h2h_man_summary(
            opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
          ),
          col_sort = c("win_pc", "P")
        )
      )
    }

    output$h2h_meetings <- {
      reactable::renderReactable(
        get_h2h_meetings(
          opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
        )
      )
    }

  })
}
