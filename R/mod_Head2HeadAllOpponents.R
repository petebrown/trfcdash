#' Head2HeadAllOpponents UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Head2HeadAllOpponents_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Overall Records"
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("h2h_records"))
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Longest Streaks"
        ),
        reactable::reactableOutput(ns("streaks"))
      )
    )
  )
}

#' Head2HeadAllOpponents Server Functions
#'
#' @noRd
mod_Head2HeadAllOpponents_server <- function(id, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ################################
    # CARD 1: HEAD-TO-HEAD RECORDS #
    ################################

    output$h2h_records <- reactable::renderReactable(
      output_h2h_records(year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games())
    )

    ################################
    # CARD 2: LONGEST STREAKS      #
    ################################

    output$streaks <- reactable::renderReactable(
      output_h2h_streaks(year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games())
    )

  })
}
