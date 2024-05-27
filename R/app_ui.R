#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bslib::page_navbar(
      fillable = FALSE,
      theme = bslib::bs_theme_update(
        bslib::bs_theme(),
        fg = "#385580",
        bg = "white",
        primary = "#385580",
        "enable-rounded" = TRUE
      ),
      title = div(
        img(
          src = "./www/images/crest.svg",
          height = 55,
          width = 55,
          style = "margin:1px 1px"
        ),
        "Tranmere Rovers: A Complete Record"),
      bslib::nav_spacer(),
      id = "nav",
      sidebar = bslib::sidebar(
        # Season Tracker sidebar
        conditionalPanel(
          "input.nav === 'Season Tracker'",
          SeasonTracker_sidebar()
        ),
        # Season Overview sidebar
        conditionalPanel(
          "input.nav === 'Season Overviews'",
          SeasonOverviews_sidebar()
        ),
        # Main head-to-head sidebar
        shiny::conditionalPanel(
          "input.nav === 'All head-to-head records'",
          Head2HeadAllOpponents_sidebar()
        ),
        # Individual head-to-head sidebar
        shiny::conditionalPanel(
          "input.nav === 'Record vs. specific team'",
          Head2HeadByOpponent_sidebar()
        ),
        # Main manager comparison sidebar
        shiny::conditionalPanel(
          "input.nav === 'Compare manager records'",
          ManagersAllManagers_sidebar()
        ),
        # Individual manager stats
        shiny::conditionalPanel(
          "input.nav === 'Individual manager stats'",
          ManagersByManager_sidebar()
        ),
        # Main player comparison tool
        shiny::conditionalPanel(
          "input.nav === 'Compare all players'",
          PlayersAllPlayers_sidebar()
        ),
        # Individual player stats
        shiny::conditionalPanel(
          "input.nav === 'Individual player stats'",
          PlayersByPlayer_sidebar()
        ),
        # Overall attendance stats
        shiny::conditionalPanel(
          "input.nav === 'Overall attendance stats'",
          AttendancesOverview_sidebar()
        ),
        # Individual attendance stats
        shiny::conditionalPanel(
          "input.nav === 'Attendance stats by season'",
          AttendancesBySeason_sidebar()
        ),
        # On This Day
        shiny::conditionalPanel(
          "input.nav === 'OTD'",
          OnThisDay_sidebar()
        )
      ),
      bslib::nav_menu(
        title = "Seasons",
        bslib::nav_panel("Season Tracker", mod_SeasonTracker_ui("SeasonTracker_ui_1")),
        bslib::nav_panel("Season Overviews", mod_SeasonOverviews_ui("SeasonOverviews_ui_1"))
      ),
      bslib::nav_menu(
        title = "H2H",
        bslib::nav_panel("All head-to-head records", mod_Head2HeadAllOpponents_ui("Head2HeadAllOpponents_ui_1")),
        bslib::nav_panel("Record vs. specific team", mod_Head2HeadByOpponent_ui("Head2HeadByOpponent_ui_1"))
      ),
      bslib::nav_menu(
        title = "Managers",
        bslib::nav_panel("Compare manager records", mod_ManagersAllManagers_ui("ManagersAllManagers_ui_1")),
        bslib::nav_panel("Individual manager stats", mod_ManagersByManager_ui("ManagersByManager_ui_1"))
      ),
      bslib::nav_menu(
        title = "Players",
        bslib::nav_panel("Compare all players", mod_PlayersAllPlayers_ui("PlayersAllPlayers_ui_1")),
        bslib::nav_panel("Individual player stats", mod_PlayersByPlayer_ui("PlayersByPlayer_ui_1"))
      ),
      bslib::nav_menu(
        title = "Attendances",
        bslib::nav_panel("Overall attendance stats", mod_AttendancesOverview_ui("AttendancesOverview_ui_1")),
        bslib::nav_panel("Attendance stats by season", mod_AttendancesBySeason_ui("AttendancesBySeason_ui_1"))
      ),
      bslib::nav_panel("OTD", mod_OnThisDay_ui("OnThisDay_ui_1"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Tranmere Rovers: A Complete Record"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
