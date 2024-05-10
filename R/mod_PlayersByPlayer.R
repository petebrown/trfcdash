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

    uiOutput(ns("pl_quick_facts")),

    h1(textOutput(ns("pl_name"))),
    p(textOutput(ns("pl_dob"))),

    uiOutput(ns("pl_image")),

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
      HTML("<a id='4sZzeQhrQhRFqyL_LbTNBA' class='gie-slideshow' href='http://www.gettyimages.com/detail/676501898' target='_blank' style='color:#a7a7a7;text-decoration:none;font-weight:normal !important;border:none;display:inline-block;'>Embed from Getty Images</a><script>window.gie=window.gie||function(c){(gie.q=gie.q||[]).push(c)};gie(function(){gie.widgets.load({id:'4sZzeQhrQhRFqyL_LbTNBA',sig:'tBu0P23sDuSnveFSg_qJV2WLLvM9BNhOc33Tw6tTW8k=',w:'594px',h:'458px',items:'676501898,85308231,128786371,96280225,676838158',caption: false ,tld:'com',is360: false })});</script><script src='//embed-cdn.gettyimages.com/widgets.js' charset='utf-8' async></script>")

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
