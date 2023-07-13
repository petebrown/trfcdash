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
    h1(textOutput(ns("name"))),

    uiOutput(ns("pl_quick_facts")),

    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Summary"
        ),
        DT::dataTableOutput(ns("pl_summary"), height = "100%", fill = FALSE)
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Season"
        ),
        DT::dataTableOutput(ns("pl_summary_by_ssn"), height = "100%", fill = FALSE)
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By League Tier"
        ),
        DT::dataTableOutput(ns("pl_summary_by_tier"), height = "100%", fill = FALSE)
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Opponent"
        ),
        DT::dataTableOutput(ns("pl_summary_by_oppo"), height = "100%", fill = FALSE)
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "Appearances"
        ),
        DT::dataTableOutput(ns("player_apps"), height = "100%", fill = FALSE)
      )
    ),
    bslib::page_fluid(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "bg-dark",
          "By Manager"
        ),
        DT::dataTableOutput(ns("pl_summary_by_mgr"), height = "100%", fill = FALSE)
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

    output$name <- renderText(player_name())

    output$pl_quick_facts <- renderUI({
      pl_value_boxes(player_name())
    })

    output$pl_summary <- {
      DT::renderDT(
        output_player_summary(player_name()),
        rownames = FALSE,
        options = list(
          dom = 'tip',
          info = FALSE,
          paging = FALSE,
          fillContainer = TRUE,
          columnDefs = list(
            list(targets = c(6, 7), className = 'dt-right')
          )
        )
      )
    }

    output$pl_summary_by_ssn <- {
      DT::renderDT(
        output_pl_summary_by_ssn(player_name()),
        rownames = FALSE,
        options = list(
          dom = 'tip',
          info = FALSE,
          paging = FALSE,
          fillContainer = TRUE,
          columnDefs = list(
            list(targets = c(4), className = 'dt-right')
          )
        )
      )
    }

    output$pl_summary_by_tier <- {
      DT::renderDT(
        output_pl_summary_by_tier(player_name()),
        rownames = FALSE,
        options = list(
          dom = 'tip',
          info = FALSE,
          paging = FALSE,
          fillContainer = TRUE,
          columnDefs = list(
            list(
              targets = c(1, 2, 3, 4, 5), className = 'dt-center'
            )
          )
        )
      )
    }

    output$pl_summary_by_oppo <- {
      DT::renderDT(
        output_pl_summary_by_opp(player_name()),
        rownames = FALSE,
        options = list(
          dom = 'tip',
          info = TRUE,
          paging = TRUE,
          pageLength = 10,
          fillContainer = TRUE,
          columnDefs = list(
            list(
              targets = c(1, 2, 3, 4, 5), className = 'dt-center'
            )
          )
        )
      )
    }

    output$player_apps <- {
      DT::renderDT(
        output_player_apps(player_name()),
        selection = 'single',
        filter = 'bottom',
        fillContainer = FALSE,
        options = list(
          fillContainer = TRUE,
          paging = TRUE,
          info = TRUE,
          scrollX = TRUE,
          columnDefs = list(
            list(orderData = 1, targets = 3),
            list(visible = FALSE, targets = c(0))
          )
        )
      )
    }

    output$pl_summary_by_mgr <- {
      DT::renderDT(
        output_pl_summary_by_mgr(player_name()),
        rownames = FALSE,
        options = list(
          dom = 'tip',
          info = FALSE,
          paging = FALSE,
          fillContainer = TRUE,
          columnDefs = list(
            list(
              targets = c(1, 2, 3, 4, 5), className = 'dt-center'
            )
          )
        )
      )
    }

  })
}
