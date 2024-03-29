#' AttendancesOverview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_AttendancesOverview_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      bslib::card_header(
        class = "bg-dark",

        "Average attendance by season"
      ),
      bslib::card_body(
        min_height = "600px",
        bslib::card(
          full_screen = TRUE,
          class = "borderless",
          bslib::card_title(
            "Average attendance by season"
          ),
          plotly::plotlyOutput(ns("plot_av_atts"))
        )
      )
    ),

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Highest attendances"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("top_atts_by_ssn"))
      )
    )

  )
}

#' AttendancesOverview Server Functions
#'
#' @noRd
mod_AttendancesOverview_server <- function(id, year_range, cup_comps){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$plot_av_atts <- plotly::renderPlotly(
      plot_av_attendances(year_range(), cup_comps())
    )

    output$top_atts_by_ssn <- reactable::renderReactable(
      top_n_attendances(
        year_range(),
        cup_comps(),
        n = 1,
        venues = "H"
      )
    )

  })
}
