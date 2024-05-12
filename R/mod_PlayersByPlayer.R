#' PlayersByPlayer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PlayersByPlayer_ui <- function(id){
  ns <- NS(id)
  tagList(

    div(
      style = "text-align: center;",
      h1(textOutput(ns("pl_name"))),
      p(textOutput(ns("pl_dob"))),
    ),

    uiOutput(ns("pl_image")),

    uiOutput(ns("pl_quick_facts")),

    uiOutput(ns("getty_image")),

    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Summary"
        ),
        reactable::reactableOutput(ns("pl_summary"))
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Season"
        ),
        reactable::reactableOutput(ns("pl_ssn_reactable"))
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Competition"
        ),
        reactable::reactableOutput(ns("pl_comp_reactable"))
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By League Tier"
        ),
        reactable::reactableOutput(ns("pl_summary_by_tier"))
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Opponent"
        ),
        reactable::reactableOutput(ns("pl_summary_by_oppo"))
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Appearances"
        ),
        reactable::reactableOutput(ns("player_apps"))
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Manager"
        ),
        reactable::reactableOutput(ns("pl_summary_by_mgr"))
      )
    )
  )
}

#' PlayersByPlayer Server Functions
#'
#' @noRd
mod_PlayersByPlayer_server <- function(id, player_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$pl_image <- renderUI({
      file_path <- map_plr_to_img(player_name())

      img(
        src = file_path,
        height = 150,
        alt = player_name()
      )
    })

    output$getty_image <- renderUI({
      if (player_name() %in% getty_img_code$menu_name) {

      img_code <- getty_img_code %>%
        dplyr::filter(menu_name == player_name()) %>%
        dplyr::pull(img_code)

      bslib::card(
        full_screen = FALSE,
        bslib::card_header(
          class = "bg-dark",
          "Player Images"
        ),
        bslib::card_body(
          class = "align-items-center",
          HTML(img_code)
        )
      )
    } else {
        NULL
      }
    })


    output$pl_name <- renderText({
      get_player_name(player_name())
    })
    output$pl_dob <- renderText({
      get_player_dob(player_name())
    })

    output$pl_quick_facts <- renderUI({
      pl_value_boxes(player_name())
    })

    output$pl_summary <- {
      reactable::renderReactable(
        output_player_summary(player_name())
      )
    }

    output$pl_ssn_reactable <- {
      reactable::renderReactable(output_plr_ssn_reactable(player_name()))
    }

    output$pl_comp_reactable <- {
      reactable::renderReactable(output_plr_comps_reactable(player_name()))
    }

    output$pl_summary_by_tier <- {
      reactable::renderReactable(
        output_pl_summary_by_tier(player_name())
      )
    }

    output$pl_summary_by_oppo <- {
      reactable::renderReactable(
        output_pl_summary_by_opp(player_name())
      )
    }

    output$player_apps <- {
      reactable::renderReactable(
        output_player_apps(player_name())
      )
    }

    output$pl_summary_by_mgr <- {
      reactable::renderReactable(
        output_pl_summary_by_mgr(player_name())
      )
    }

  })
}
