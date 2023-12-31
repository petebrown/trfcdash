#' PlayersAllPlayers UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PlayersAllPlayers_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Player Records"
        ),
        reactable::reactableOutput(ns("player_records"))
      ),


      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Player Streaks"
        ),
        reactable::reactableOutput(ns("player_streaks"))
      ),


      bslib::layout_column_wrap(
        width = 1/2,
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-dark",
            "Player Debuts"
          ),
          reactable::reactableOutput(ns("player_debuts"))
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-dark",
            "Oldest Players"
          ),
          reactable::reactableOutput(ns("oldest_players"))
        )
      )
    )
  )
}

#' PlayersAllPlayers Server Functions
#'
#' @noRd
mod_PlayersAllPlayers_server <- function(id, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, min_games){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$player_records <- {
      reactable::renderReactable(
        output_player_records(year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games())
      )
    }

    output$player_streaks <- {
      reactable::renderReactable(
        output_all_plr_streaks(year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), min_games())
      )
    }

    output$player_debuts <- {
      reactable::renderReactable(
        get_pl_debuts()
      )
    }

    output$oldest_players <- {
      reactable::renderReactable(
        get_oldest_players(n=1)
      )
    }

  })
}
