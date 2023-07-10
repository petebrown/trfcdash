#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  mod_SeasonTracker_server("SeasonTracker_ui_1")

  mod_Head2HeadByOpponent_server("Head2HeadByOpponent_ui_1")
  mod_Head2HeadAllOpponents_server("Head2HeadAllOpponents_ui_1")

  mod_ManagersByManager_server("ManagersByManager_ui_1")
  mod_ManagersAllManagers_server("ManagersAllManagers_ui_1")

  mod_PlayersByPlayer_server("PlayersByPlayer_ui_1")
  mod_PlayersAllPlayers_server("PlayersAllPlayers_ui_1")

  mod_AttendancesOverview_server("AttendancesOverview_ui_1")
  mod_AttendancesBySeason_server("AttendancesBySeason_ui_1")

  mod_OnThisDay_server("OnThisDay_ui_1")

  # Receive user inputs from Season Tracker sidebar
  SeasonTracker_inputs <- mod_SeasonTracker_sidebar_server("SeasonTracker_sidebar_ui_1")
  # Send user inputs from Season Tracker sidebar to Season Tracker server
  mod_SeasonTracker_server(
    "SeasonTracker_ui_1",
    selected_seasons = SeasonTracker_inputs[[1]],
    n_fixtures = SeasonTracker_inputs[[2]]
  )

  # Receive user inputs from Head-to-Head (all opponents) sidebar
  Head2HeadAll_inputs <- mod_Head2HeadAllOpponents_sidebar_server("Head2HeadAllOpponents_sidebar_ui_1")
  # Send user inputs from Head-to-Head (all opponents) sidebar to Head-to-Head (all opponents) server
  mod_Head2HeadAllOpponents_server(
    "Head2HeadAllOpponents_ui_1",
    year_range = Head2HeadAll_inputs[[1]],
    league_tiers = Head2HeadAll_inputs[[2]],
    cup_comps = Head2HeadAll_inputs[[3]],
    venue_options = Head2HeadAll_inputs[[4]],
    min_games = Head2HeadAll_inputs[[5]]
  )
}
