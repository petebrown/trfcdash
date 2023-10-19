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
        "Managerial Records"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("h2h_man_records"))
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

    output$h2h_man_records <- {
      reactable::renderReactable(
        mgr_img_table(
          get_h2h_man_summary(
            opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
          ),
          col_sort = "win_pc"
        )
      )
    }

  })
}
