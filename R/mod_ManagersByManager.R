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

    div(
      class="row",
      style="margin-bottom: 2rem;",
      div(
        class="col-sm-auto d-flex align-items-center",
        style="text-align: left; margin-left: 1rem",
        h1(textOutput(ns("manager_name")), class = "display-1 opponent-title")
      ),
      div(
        class="col align-items-center",
        style="margin-right: 1rem;",
        uiOutput(ns("manager_img"))
      )
    ),

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Overall Record"
      ),
      bslib::layout_sidebar(
        fillable = FALSE,
        sidebar = bslib::sidebar(
          position = "left",
          width = 200,
          bg = "#4c668d",
          class = "card-sidebar",
          open = FALSE,
          radioButtons(
            inputId = ns("mgr_recs_inc_cup_games"),
            label = "Include cup games?",
            choices = c("Yes", "No"),
            selected = "Yes",
            inline = TRUE
          ),
          hr(),
          radioButtons(
            inputId = ns("mgr_recs_pens_as_draw"),
            label = "Treat one-off cup games decided by penalty shoot-out as draws?",
            choices = c("Yes", "No"),
            selected = "Yes",
            inline = TRUE
          )
        ),
        radioButtons(
          inputId = ns("record_type"),
          label = NULL,
          choiceNames = list(
            "Overall",
            "By season",
            "By opponent",
            "By competition"
          ),
          choiceValues = list(
            "overall",
            "season",
            "opposition",
            "competition"
          ),
          inline = TRUE
        ),
        reactable::reactableOutput(ns("mgr_records"))
      )
    ),


    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Overall Record by Season"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_summary_by_ssn_reactable"))
      )
    ),

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Streaks"
      ),
      bslib::layout_sidebar(
        fillable = FALSE,
        sidebar = bslib::sidebar(
          position = "left",
          width = 200,
          bg = "#4c668d",
          class = "card-sidebar",
          open = FALSE,
          radioButtons(
            inputId = ns("mgr_streaks_inc_cup_games"),
            label = "Include cup games?",
            choices = c("Yes", "No"),
            selected = "Yes",
            inline = TRUE
          ),
          hr(),
          radioButtons(
            inputId = ns("mgr_streaks_pens_as_draw"),
            label = "Treat one-off cup games decided by penalty shoot-out as draws?",
            choices = c("Yes", "No"),
            selected = "Yes",
            inline = TRUE
          )
        ),
        radioButtons(
          inputId = ns("streak_type"),
          label = NULL,
          choiceNames = list(
            "Overall",
            "By season",
            "By opponent"
          ),
          choiceValues = list(
            "overall",
            "season",
            "opposition"
          ),
          inline = TRUE
        ),
        reactable::reactableOutput(ns("mgr_streaks"))
      )
    ),


    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Players Used"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_player_recs"))
      )
    ),

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Head-to-Head Records"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_h2h_recs"))
      )
    ),

    bslib::card(
      bslib::card_header(
        class = "bg-dark",
        "Games Managed"
      ),
      bslib::card_body(
        reactable::reactableOutput(ns("mgr_results"))
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

    output$manager_name <- renderText({
      manager_name()
    })

    output$manager_img <- renderUI({
      manager_name <- manager_name()

      title <- h1(manager_name)

      if (isTRUE(manager_name == "No manager")) {
        file_path = "./www/images/crest.svg"
      } else {
        img_dir = "./www/images/managers"

        if (stringr::str_detect(manager_name, " & ")) {
          manager_name <- stringr::str_split_i(manager_name(), " &", 1)
        }

        file_name = paste0(
          stringr::str_to_lower(stringr::str_replace_all(manager_name, " ", "-")),
          ".jpg"
        )

        file_path = file.path(img_dir, file_name)
      }


      img(
        src = file_path,
        class = if(manager_name != "No manager") {"rounded-circle"},
        width = 200,
        height = 200,
        alt = manager_name,
        class = c("float-right", if(manager_name != "No manager") {"rounded-circle"})
      )

    })

    record_type <- reactive({
      input$record_type
    })

    recs_inc_cup_games <- reactive({
      input$mgr_recs_inc_cup_games
    })
    recs_pens_as_draw <- reactive({
      input$mgr_recs_pens_as_draw
    })

    output$mgr_records <- {
      reactable::renderReactable(
        expr = output_mgr_records(
          manager_name(),
          record_type(),
          recs_inc_cup_games(),
          recs_pens_as_draw()
        )
      )
    }

    output$mgr_summary_by_ssn_reactable <- {
      reactable::renderReactable(
        expr = get_mgr_summary_by_ssn_reactable(manager_name())
      )
    }

    streak_type <- reactive({
      input$streak_type
    })

    streaks_inc_cup_games <- reactive({
      input$mgr_streaks_inc_cup_games
    })
    streaks_pens_as_draw <- reactive({
      input$mgr_streaks_pens_as_draw
    })

    output$mgr_streaks <- {
      reactable::renderReactable(
        expr = output_mgr_streaks(manager_name(), streak_type(), streaks_inc_cup_games(), streaks_pens_as_draw())
      )
    }

    output$mgr_results <- {
      reactable::renderReactable(
        expr = output_mgr_games(manager_name())
      )
    }


    output$mgr_player_recs <- {
      reactable::renderReactable(
        expr = output_mgr_plr_records(manager_name())
      )
    }

    output$mgr_h2h_recs <- {
      reactable::renderReactable(
        expr = get_mgr_h2h_summary(manager_name())
      )
    }

  })
}
