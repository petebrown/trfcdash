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
        "Attack and Defence"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("att_and_def"), height = "auto"),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark d-flex justify-content-between",
        "Biggest Wins",
        wins_popover_options(c(
          ns("wins_react_min_diff"),
          ns("wins_react_show_details")
        ))
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("biggest_wins"), height = "auto"),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "bg-dark d-flex justify-content-between",
        "Biggest Defeats",
        defeats_popover_options(c(
          ns("defeats_react_min_diff"),
          ns("defeats_react_show_details")
        ))
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("biggest_defeats"), height = "auto"),
      )
    ),

    bslib::layout_column_wrap(
      width = 1/2,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Player Counts"
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("player_stats"), height = "auto"),
        )
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Discipline"
        ),
        bslib::card_body(
          reactable::reactableOutput(ns("player_discipline"), height = "auto"),
        )
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

    base_df = reactive({
      base_season_overviews_df(
        year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
      )
    })

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

    output$att_and_def <- {
      reactable::renderReactable(
        get_attack_and_defend(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

    min_win_diff <- reactive({
      input$wins_react_min_diff
    })
    wins_show_details <- reactive({
      input$wins_react_show_details
    })

    biggest_wins_df <- reactive({
      get_biggest_wins(
        base_df(), min_win_diff()
      )
    })

    output$biggest_wins <- {
      reactable::renderReactable({
        results_with_subtable(
          df = biggest_wins_df(),
          drop_cols = c('outcome'),
          show_details = wins_show_details()
        )
      })
    }

    min_defeat_diff <- reactive({
      input$defeats_react_min_diff
    })
    defeats_show_details <- reactive({
      input$defeats_react_show_details
    })

    biggest_defeats_df <- reactive({
      get_biggest_defeats(
        base_df(), min_defeat_diff()
      )
    })

    output$biggest_defeats <- {
      reactable::renderReactable({
        results_with_subtable(
          df = biggest_defeats_df(),
          drop_cols = c('outcome'),
          show_details = defeats_show_details()
        )
      })
    }

    output$player_stats <- {
      reactable::renderReactable(
        get_season_plr_stats(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

    output$player_discipline <- {
      reactable::renderReactable(
        get_season_discipline(
          year_range(), league_tiers(), includePlayOffs(), cup_comps(), pens_as_draw(), venue_options(), game_range()
        )
      )
    }

  })
}
