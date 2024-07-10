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
    bslib::page_fluid(

      uiOutput(ns("pl_quick_facts")),

      div(
        class="row",
        style="margin-bottom: 2rem;",
        div(
          class="col-sm-auto d-flex align-items-left",
          style="text-align: left; margin-left: 1rem; flex-direction: column;",
          h1(
            class = "display-1 opponent-title",
            style = "margin-bottom: 0.05rem",
            textOutput(ns("pl_name"))
          ),
          div(
            span(
              style = "display:flex; color:grey; font-size:xx-large; font-weight: 300;",
              textOutput(ns("pl_pos"))
            ),
            span(
              style = "display:flex; margin-top: 0.75rem; color:#909090; font-size:smaller; font-weight: 300;",
              span(style = "margin-right: 0.25rem; font-weight: 400;", "Born"),
              textOutput(ns("pl_dob"))
            )
          )
        ),
        div(
          class="col align-items-center",
          style="margin-right: 1rem;",
          uiOutput(ns("pl_image"))
        )
      ),

      # div(
      #   p(
      #     textOutput(ns("pl_name"), inline=TRUE),
      #     style = "text-align: center; font-size: 6rem; font-weight: 900;"
      #   ),
      #
      #   p(textOutput(ns("pl_dob"))),
      #   p("[Place of birth]"),
      #   p("[Position]"),
      #   p("[Nationality]"),
      #   p("[Height]"),
      #   p("[No. spells]"),
      #   p("[Date signed]")
      # ),
      #
      # uiOutput(ns("pl_image")),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Summary"
        ),
        reactable::reactableOutput(ns("pl_summary"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Season"
        ),
        reactable::reactableOutput(ns("pl_ssn_reactable"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Competition"
        ),
        reactable::reactableOutput(ns("pl_comp_reactable"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By League Tier"
        ),
        reactable::reactableOutput(ns("pl_summary_by_tier"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Opponent"
        ),
        reactable::reactableOutput(ns("pl_summary_by_oppo"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Appearances"
        ),
        reactable::reactableOutput(ns("player_apps"))
      ),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Manager"
        ),
        reactable::reactableOutput(ns("pl_summary_by_mgr"))
      ),

      uiOutput(ns("getty_image"))
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
        height = 200,
        alt = player_name(),
        class = "float-right"
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
      dob = get_player_dob(player_name())
      dob = as.Date(dob, format = "%Y-%m-%d")
      dob = format(dob, "%e %B, %Y")

    })
    output$pl_pos <- renderText({
      map_pos_to_id(player_name())
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
