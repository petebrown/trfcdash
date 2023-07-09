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
          DT::dataTableOutput(ns("h2h_records"))
        )
      ),
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Longest Streaks"
        ),
        bslib::card_body(
          DT::dataTableOutput(ns("streaks"))
        )
      )
    )
  )
}

#' Head2HeadAllOpponents Server Functions
#'
#' @noRd
mod_Head2HeadAllOpponents_server <- function(id, year_range){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ################################
    # CARD 1: HEAD-TO-HEAD RECORDS #
    ################################

    output$h2h_records <- DT::renderDT(
      output_h2h_records(year_range()),
      rownames = FALSE,
      options = list(
        pageLength = 5,
        dom = 'tip',
        info = FALSE,
        paging = FALSE,
        fillContainer = TRUE
      )
    )

    ################################
    # CARD 2: LONGEST STREAKS      #
    ################################

    output$streaks <- DT::renderDT(
      results_dataset,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        dom = 'tip',
        info = FALSE,
        paging = FALSE,
        fillContainer = TRUE
      )
    )


  })
}
