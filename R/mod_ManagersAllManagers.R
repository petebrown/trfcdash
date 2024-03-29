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
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Longest Streaks"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_streaks"), height = "auto"),
      )
    )

  )
}

#' ManagersAllManagers Server Functions
#'
#' @noRd
mod_ManagersAllManagers_server <- function(id, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games, inc_caretakers){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$mgr_records <- {
      reactable::renderReactable(
        output_all_mgr_records(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games(), inc_caretakers()
        )
      )
    }


    output$mgr_streaks <- {
      reactable::renderReactable(
        output_all_mgr_streaks(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games(), inc_caretakers()
        )
      )
    }

  })
}
