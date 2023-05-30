#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  mod_SeasonTracker_server("SeasonTracker_1")

  mod_Head2HeadByOpponent_server("Head2HeadByOpponent_1")
  mod_Head2HeadAllOpponents_server("Head2HeadAllOpponents_1")

  mod_ManagersByManager_server("ManagersByManager_1")
  mod_ManagersAllManagers_server("ManagersAllManagers_1")

  mod_PlayersByPlayer_server("PlayersByPlayer_1")
  mod_PlayersAllPlayers_server("PlayersAllPlayers_1")

  mod_AttendancesOverview_server("AttendancesOverview_1")
  mod_AttendancesBySeason_server("AttendancesBySeason_1")

  mod_OnThisDay_server("OnThisDay_1")

}
