date_today = function() {
  return (Sys.Date())
}

get_api_url = function(endpoint) {
  api_base = 'https://web-cdn.api.bbci.co.uk/wc-poll-data/container'

  url = stringr::str_glue('{api_base}/{endpoint}')

  return (url)
}

get_fixtures_url = function(start_date=date_today(), end_date=FALSE) {
  if (end_date == FALSE) {
    end_date = start_date
  }

  fixtures = stringr::str_glue('sport-data-scores-fixtures?selectedEndDate={end_date}&selectedStartDate={start_date}&todayDate={date_today()}&urn=urn%3Abbc%3Asportsdata%3Afootball%3Ateam%3Atranmere-rovers&useSdApi=false')

  url = get_api_url(fixtures)

  return (url)
}

get_table_url = function(game_date=date_today(), match_id) {
  table = stringr::str_glue('football-table?globalContainerPolling=true&matchDate={game_date}&matchUrn=urn%3Abbc%3Asportsdata%3Afootball%3Aevent%3A{match_id}')

  url = get_api_url(table)

  return (url)
}

get_json = function(url) {
  req = httr2::request(url)

  resp = httr2::req_perform(req)

  json = resp |>
    httr2::resp_body_json()

  return (json)
}

game_date = '2024-08-24'

fixtures_json = get_json(get_fixtures_url(game_date))

resource_id = fixtures_json$eventGroups[[1]]$secondaryGroups[[1]]$events[[1]]$tipoTopicId

match_id = fixtures_json$eventGroups[[1]]$secondaryGroups[[1]]$events[[1]]$id

table_json = get_json(get_table_url(game_date, match_id))

league_name = table_json[["tournaments"]][[1]][["name"]]

participants = table_json[["tournaments"]][[1]][["stages"]][[1]][["rounds"]][[1]][["participants"]]

extract_participant_info <- function(participant) {
  data.frame(
    season = max(results_dataset$season),
    urn = participant$urn,
    rank = participant$rank,
    shortName = participant$shortName,
    rankPrevious = participant$rankPrevious,
    name = participant$name,
    points = participant$points,
    matchesPlayed = participant$matchesPlayed,
    wins = participant$wins,
    losses = participant$losses,
    draws = participant$draws,
    goalsScoredFor = participant$goalsScoredFor,
    goalsScoredAgainst = participant$goalsScoredAgainst,
    goalDifference = participant$goalDifference,
    formGuide = paste(sapply(participant$formGuide, function(fg) fg$value), collapse = ", "),
    url = participant$url
  )
}

# Apply the function to each participant and combine into a dataframe
participants_df <- do.call(rbind, lapply(participants, extract_participant_info))
