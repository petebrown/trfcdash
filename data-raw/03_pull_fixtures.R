# url <- "https://football-web-pages1.p.rapidapi.com/fixtures-results.json"
#
# queryString <- list(
#   team = "66"
# )
#
# response <- httr::VERB(
#   "GET",
#   url,
#   query = queryString,
#   httr::add_headers('x-rapidapi-key' = '78a5cb3dc2msh512a63ac2805904p19c7aajsn09952ea8960a', 'x-rapidapi-host' = 'football-web-pages1.p.rapidapi.com'),
#   httr::content_type("application/octet-stream")
# )
#
# f_json <- httr::content(response, "text")
# f_json <- jsonlite::fromJSON(f_json)
# f_json <- jsonlite::flatten(f_json$`fixtures-results`[2]$matches)
#
# fixtures <- f_json %>%
#   dplyr::rename(
#     home_team = `home-team.name`,
#     away_team = `away-team.name`,
#     competition = competition.name,
#   ) %>%
#   dplyr::mutate(
#     season = '2025/26',
#     game_date = as.Date(date, format = "%Y-%m-%d"),
#     venue = ifelse(home_team == 'Tranmere Rovers', 'H', 'A'),
#     opposition = ifelse(home_team == 'Tranmere Rovers', away_team, home_team),
#     competition = ifelse(competition == 'Sky Bet League Two', 'League Two', competition),
#     game_type = ifelse(competition == 'League Two', 'League', 'Cup')
#   ) %>%
#   dplyr::select(
#     season,
#     game_date,
#     opposition,
#     venue,
#     competition,
#     game_type
#   )

unplayed_fixtures <- fixtures %>%
  dplyr::mutate(
    game_date = dplyr::case_when(opposition=='Newport County' & venue=='A' ~ as.Date('2026-05-03'), .default=game_date)
  ) %>%
  dplyr::arrange(game_date) %>%
  dplyr::filter(
    !game_date %in% results_dataset$game_date,
    game_type == 'League'
  )

usethis::use_data(
  fixtures,
  unplayed_fixtures,
  overwrite = TRUE
)
