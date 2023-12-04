#' SeasonOverviews UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_SeasonOverviews_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Season Records"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("ssn_records"), height = "auto"),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Longest Streaks"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("ssn_streaks"), height = "auto"),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Goal Margins"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("goal_margins"), height = "auto"),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark",
        "Players Used"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("player_stats"), height = "auto"),
      )
    )

  )
}

#' SeasonOverviews Server Functions
#'
#' @noRd
mod_SeasonOverviews_server <- function(id, year_range, league_tiers, includePlayOffs, cup_comps, pens_as_draw, venue_options, game_range){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$ssn_streaks <- {
      reactable::renderReactable(
        get_season_streaks(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

    output$ssn_records <- {
      reactable::renderReactable(
        get_season_records(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

    output$goal_margins <- {
      reactable::renderReactable(
        get_goal_margins(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

    output$player_stats <- {
      reactable::renderReactable(
        get_season_plr_stats(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

  })
}
