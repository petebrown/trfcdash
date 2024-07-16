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

        div(
          style = "margin-bottom: 1.5rem;",
          h1(textOutput(ns("otd_string")))
        ),

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

        bslib::layout_column_wrap(
          width = 1/2,
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
  )
}

#' OnThisDay Server Functions
#'
#' @noRd
mod_OnThisDay_server <- function(id, otd_date, otd_inc_year){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    otd_day <- reactive({
      format(otd_date(), "%d")
    })
    otd_month <- reactive({
      lubridate::month(otd_date(), label = TRUE, abbr = FALSE)
    })
    otd_year <- reactive({
      lubridate::year(otd_date())
    })
    output$otd_string <- renderText({
      paste("Results, birthdays and debuts on", otd_day(), otd_month(), ifelse(otd_inc_year() == "Yes", otd_year(), ""))
    })



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
