#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # bslib::bs_themer()

  mod_SeasonTracker_server("SeasonTracker_ui_1")
  mod_SeasonOverviews_server("SeasonOverviews_ui_1")

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
    selected_seasons = SeasonTracker_inputs[[1]]
  )

  # Receive user inputs from Season Overviews sidebar
  SeasonOverviews_inputs <- mod_SeasonOverviews_sidebar_server("SeasonOverviews_sidebar_ui_1")
  # Send user inputs from Season Overviews sidebar to Season server
  mod_SeasonOverviews_server(
    "SeasonOverviews_ui_1",
    year_range = SeasonOverviews_inputs[[1]],
    league_tiers = SeasonOverviews_inputs[[2]],
    includePlayOffs = SeasonOverviews_inputs[[3]],
    cup_comps = SeasonOverviews_inputs[[4]],
    pens_as_draw = SeasonOverviews_inputs[[5]],
    venue_options = SeasonOverviews_inputs[[6]],
    game_range = SeasonOverviews_inputs[[7]]
  )

  # Receive user inputs from Head-to-Head (individual) sidebar
  Head2HeadByOpponent_inputs <- mod_Head2HeadByOpponent_sidebar_server("Head2HeadByOpponent_sidebar_ui_1")
  # Send user inputs from Head-to-Head (individual) sidebar to Head-to-Head (individual) server
  mod_Head2HeadByOpponent_server(
    "Head2HeadByOpponent_ui_1",
    opponent = Head2HeadByOpponent_inputs[[1]],
    year_range = Head2HeadByOpponent_inputs[[2]],
    league_tiers = Head2HeadByOpponent_inputs[[3]],
    includePlayOffs = Head2HeadByOpponent_inputs[[4]],
    cup_comps = Head2HeadByOpponent_inputs[[5]],
    pens_as_draw = Head2HeadByOpponent_inputs[[6]],
    venue_options = Head2HeadByOpponent_inputs[[7]]
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


  # Receive user inputs from Players (all players) sidebar
  PlayersAllPlayers_inputs <- mod_PlayersAllPlayers_sidebar_server("PlayersAllPlayers_sidebar_ui_1")
  # Send user inputs from Players (all players) sidebar to Players (all players) server
  mod_PlayersAllPlayers_server(
    "PlayersAllPlayers_ui_1",
    year_range = PlayersAllPlayers_inputs[[1]],
    league_tiers = PlayersAllPlayers_inputs[[2]],
    includePlayOffs = PlayersAllPlayers_inputs[[3]],
    cup_comps = PlayersAllPlayers_inputs[[4]],
    pens_as_draw = PlayersAllPlayers_inputs[[5]],
    venue_options = PlayersAllPlayers_inputs[[6]],
    min_games = PlayersAllPlayers_inputs[[7]]
  )


  # Receive user inputs from Players (individual) sidebar
  PlayersByPlayer_inputs <- mod_PlayersByPlayer_sidebar_server("PlayersByPlayer_sidebar_ui_1")
  # Send user inputs from Players sidebar to Players server
  mod_PlayersByPlayer_server(
    "PlayersByPlayer_ui_1",
    player_name = PlayersByPlayer_inputs[[1]]
  )


  # Receive user inputs from Head-to-Head (all opponents) sidebar
  Head2HeadAll_inputs <- mod_Head2HeadAllOpponents_sidebar_server("Head2HeadAllOpponents_sidebar_ui_1")
  # Send user inputs from Head-to-Head (all opponents) sidebar to Head-to-Head (all opponents) server
  mod_Head2HeadAllOpponents_server(
    "Head2HeadAllOpponents_ui_1",
    year_range = Head2HeadAll_inputs[[1]],
    league_tiers = Head2HeadAll_inputs[[2]],
    includePlayOffs = Head2HeadAll_inputs[[3]],
    cup_comps = Head2HeadAll_inputs[[4]],
    pens_as_draw = Head2HeadAll_inputs[[5]],
    venue_options = Head2HeadAll_inputs[[6]],
    min_games = Head2HeadAll_inputs[[7]]
  )


  # Receive user inputs from Managers (all managers) sidebar
  ManagersByManager_inputs <- mod_ManagersByManager_sidebar_server("ManagersByManager_sidebar_ui_1")
  # Send user inputs from Managers sidebar to Managers server
  mod_ManagersByManager_server(
    "ManagersByManager_ui_1",
    manager_name = ManagersByManager_inputs[[1]]
  )


  # Receive user inputs from Managers (all managers) sidebar
  ManagersAllManagers_inputs <- mod_ManagersAllManagers_sidebar_server("ManagersAllManagers_sidebar_ui_1")
  # Send user inputs from Managers (all managers) sidebar to Manager (all managers) server
  mod_ManagersAllManagers_server(
    "ManagersAllManagers_ui_1",
    year_range = ManagersAllManagers_inputs[[1]],
    league_tiers = ManagersAllManagers_inputs[[2]],
    includePlayOffs = ManagersAllManagers_inputs[[3]],
    cup_comps = ManagersAllManagers_inputs[[4]],
    pens_as_draw = ManagersAllManagers_inputs[[5]],
    venue_options = ManagersAllManagers_inputs[[6]],
    min_games = ManagersAllManagers_inputs[[7]],
    inc_caretakers = ManagersAllManagers_inputs[[8]]
  )

  # Receive user inputs from On This Day sidebar
  OnThisDay_inputs <- mod_OnThisDay_sidebar_server("OnThisDay_sidebar_ui_1")
  # Send user inputs from On This Day sidebar to On This Day server
  mod_OnThisDay_server(
    "OnThisDay_ui_1",
    otd_date = OnThisDay_inputs[[1]],
    otd_inc_year = OnThisDay_inputs[[2]]
  )


  # Receive user inputs from AttendancesOverview sidebar
  AttendancesOverview_inputs <- mod_AttendancesOverview_sidebar_server("AttendancesOverview_sidebar_ui_1")
  # Send user inputs from AttendancesOverview sidebar to AttendancesOverview server
  mod_AttendancesOverview_server(
    "AttendancesOverview_ui_1",
    year_range = AttendancesOverview_inputs[[1]]
  )

}
