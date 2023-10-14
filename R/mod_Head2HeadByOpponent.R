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
        reactable::reactableOutput(ns("h2h_record"))
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

    output$h2h_record <- {
      reactable::renderReactable(
        reactable::reactable(
          get_h2h_summary(
            opponent(), year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options()
          )
        )
      )
    }

  })
}
