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
          DT::dataTableOutput(ns("h2h_records"), height = "100%", fill = FALSE)
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Longest Streaks"
        ),
        DT::dataTableOutput(ns("streaks"), height = "100%", fill = FALSE)
      )
    )
  )
}

#' Head2HeadAllOpponents Server Functions
#'
#' @noRd
mod_Head2HeadAllOpponents_server <- function(id, year_range, league_tiers, cup_comps, venue_options, min_games){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ################################
    # CARD 1: HEAD-TO-HEAD RECORDS #
    ################################

    output$h2h_records <- DT::renderDT(
      output_h2h_records(year_range(), league_tiers(), cup_comps(), venue_options(), min_games()),
      selection = 'single',
      filter = 'bottom',
      rownames = FALSE,
      fillContainer = FALSE,
      options = list(
        fillContainer = TRUE,
        paging = TRUE,
        info = TRUE,
        scrollX = TRUE
      )
    )

    ################################
    # CARD 2: LONGEST STREAKS      #
    ################################

    output$streaks <- DT::renderDT(
      output_h2h_streaks(year_range(), league_tiers(), cup_comps(), venue_options(), min_games()),
      selection = 'single',
      filter = 'bottom',
      rownames = FALSE,
      fillContainer = FALSE,
      options = list(
        fillContainer = TRUE,
        paging = TRUE,
        info = TRUE,
        scrollX = TRUE
      )
    )

  })
}
