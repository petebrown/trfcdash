#' OnThisDay UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OnThisDay_ui <- function(id){
  ns <- NS(id)
  tagList(
    tagList(
      bslib::page_fluid(
        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-dark",
            "On this Day: Matches"
          ),
          reactable::reactableOutput(ns("otd_results"))
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-dark",
            "On this Day: Record"
          ),
          reactable::reactableOutput(ns("otd_record"))
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-dark",
            "On this Day: Player Debuts"
          ),
          reactable::reactableOutput(ns("otd_debuts"))
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            class = "bg-dark",
            "On this Day: Player Birthdays"
          ),
          reactable::reactableOutput(ns("otd_birthdays"))
        )
      )
    )
  )
}

#' OnThisDay Server Functions
#'
#' @noRd
mod_OnThisDay_server <- function(id, otd_date, otd_inc_year){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$otd_results <- {
      reactable::renderReactable(
        get_otd_results(
          otd_date(), otd_inc_year(), as_reactable="Yes"
        )
      )
    }

    output$otd_record <- {
      reactable::renderReactable(
        get_otd_record(
          otd_date(), otd_inc_year()
        )
      )
    }

    output$otd_debuts <- {
      reactable::renderReactable(
        get_otd_debuts(
          otd_date(), otd_inc_year()
        )
      )
    }

    output$otd_birthdays <- {
      reactable::renderReactable(
        get_otd_birthdays(
          otd_date(), otd_inc_year()
        )
      )
    }

  })
}
