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

      uiOutput(ns("manager_img")),

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

    output$manager_img <- renderUI({
      manager_name <- manager_name()

      title <- h1(manager_name)

      img_dir = "./www/images/managers"

      if (stringr::str_detect(manager_name, " & ")) {
        manager_name <- stringr::str_split_i(manager_name(), " &", 1)
      }

      file_name = paste0(
        stringr::str_to_lower(stringr::str_replace_all(manager_name, " ", "-")),
        ".jpg"
      )

      file_path = file.path(img_dir, file_name)

      bslib::card(
        h1(manager_name()),
        img(
          src = file_path,
          class = "rounded-circle",
          width = 150,
          height = 150,
          alt = manager_name
        )
      )
    })

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
