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

    shinydashboardPlus::dashboardPage(
      header = shinydashboardPlus::dashboardHeader(
        title = "Tranmere Rovers F.C."
      ),

      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem(
            "Season Tracker", tabName = "SeasonTracker", icon = icon("chart-line"),
            selected = TRUE
          ),
          shinydashboard::menuItem(
            "Head-to-Head Records", icon = icon("futbol"),
            shinydashboard::menuSubItem("By Opponent", tabName = "Head2HeadByOpponent"),
            shinydashboard::menuSubItem("All Opponents", tabName = "Head2HeadAllOpponents")
          ),
          shinydashboard::menuItem(
            "Managers", icon = icon("user-tie"),
            shinydashboard::menuSubItem("By Manager", tabName = "ManagersByManager"),
            shinydashboard::menuSubItem("All Managers", tabName = "ManagersAllManagers")
          ),
          shinydashboard::menuItem(
            "Players (1996-)", icon = icon("person-running"),
            shinydashboard::menuSubItem("By Player", tabName = "PlayersByPlayer"),
            shinydashboard::menuSubItem("All Players", tabName = "PlayersAllPlayers")
          ),
          shinydashboard::menuItem(
            "Attendances", icon = icon("users-line"),
            shinydashboard::menuSubItem("Overview", tabName = "AttendancesOverview"),
            shinydashboard::menuSubItem("By Season", tabName = "AttendancesBySeason")
          ),
          shinydashboard::menuItem(
            "On This Day", tabName = "OnThisDay", icon = icon("calendar-days")
          )
        )
      ),

      body = shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("SeasonTracker", mod_SeasonTracker_ui("SeasonTracker_ui_1")),
          shinydashboard::tabItem("Head2HeadByOpponent", mod_Head2HeadByOpponent_ui("Head2HeadByOpponent_1")),
          shinydashboard::tabItem("Head2HeadAllOpponents", mod_Head2HeadAllOpponents_ui("Head2HeadAllOpponents_1")),



          shinydashboard::tabItem("ManagersByManager", mod_ManagersByManager_ui("ManagersByManager_1")),



          shinydashboard::tabItem("ManagersAllManagers", mod_ManagersAllManagers_ui("ManagersAllManagers_1")),
          shinydashboard::tabItem("PlayersByPlayer", mod_PlayersByPlayer_ui("PlayersByPlayer_1")),
          shinydashboard::tabItem("PlayersAllPlayers", mod_PlayersAllPlayers_ui("")),
          shinydashboard::tabItem("AttendancesOverview", mod_AttendancesOverview_ui("AttendancesOverview_1")),
          shinydashboard::tabItem("AttendancesBySeason", mod_AttendancesBySeason_ui("AttendancesBySeason_1")),
          shinydashboard::tabItem("OnThisDay", mod_OnThisDay_ui("OnThisDay_ui_1"))
        )
      )
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
      app_title = "trfcdash"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
