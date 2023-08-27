#' ManagersByManager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ManagersByManager_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::page_fluid(
      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Overall Record"
        ),
        bslib::card_body(
          gt::gt_output(ns("mgr_summary_overall"))
        )
      ),

      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Overall Record by Competition"
        ),
        bslib::card_body(
          gt::gt_output(ns("mgr_summary_by_comp"))
        )
      ),

      bslib::card(
        bslib::card_header(
          class = "bg-dark",
          "Overall Record by Season"
        ),
        bslib::card_body(
          gt::gt_output(ns("mgr_summary_by_ssn"))
        )
      )
    )
  )
}

#' ManagersByManager Server Functions
#'
#' @noRd
mod_ManagersByManager_server <- function(id, manager_name){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$mgr_summary_overall <- {
      gt::render_gt(
        expr = get_mgr_summary_overall(manager_name()),
        width = "100%"
      )
    }

    output$mgr_summary_by_ssn <- {
      gt::render_gt(
        expr = get_mgr_summary_by_ssn(manager_name()),
        width = "100%"
      )
    }

    output$mgr_summary_by_comp <- {
      gt::render_gt(
        expr = get_mgr_summary_by_comp(manager_name()),
        width = "100%"
      )
    }

  })
}
