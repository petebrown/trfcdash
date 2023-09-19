#' ManagersAllManagers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ManagersAllManagers_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      min_height = "100%",
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Overall Records"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_records"))
      )
    ),


    bslib::card(
      min_height = "100%",
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Longest Streaks"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_streaks"))
      )
    )

  )
}

#' ManagersAllManagers Server Functions
#'
#' @noRd
mod_ManagersAllManagers_server <- function(id, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$mgr_records <- {
      reactable::renderReactable(
        output_all_mgr_records(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games()
        )
      )
    }


    output$mgr_streaks <- {
      reactable::renderReactable(
        output_all_mgr_streaks(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games()
        )
      )
    }

  })
}
