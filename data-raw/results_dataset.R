## code to prepare `results_dataset` dataset goes here


###########################################################
#  IMPORT results_df                                      #
#  Import full data set scraped from 11v11                #
###########################################################

results_df <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv",
  col_select = c(
    "game_date",
    "season",
    "opposition",
    "venue",
    "home_team",
    "away_team",
    "score",
    "outcome",
    "competition",
    "generic_comp",
    "goals_for",
    "goals_against",
    "attendance",
    "league_tier",
    "game_type",
    "ssn_game_no",
    "ssn_comp_game_no",
    "weekday",
    "manager"
  ),
  show_col_types = FALSE)


############################################################
#  IMPORT results_mini                                     #
#  Import game-by-game league positions and point tallies  #
#  N.B. League games ONLY                                  #
############################################################

results_mini <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv",
  col_select = c(
    "game_date",
    "ranking",
    "pts"
  ),
  show_col_types = FALSE) %>%
  dplyr::rename(
    league_pos = ranking
  )


###########################################################
#  IMPORT extra_details                                   #
#  Import extra cup game data scraped from 11v11          #
#  N.B. Cup games ONLY                                    #
###########################################################

extra_details <- readr::read_csv(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/11v11-extra-details/cup_details.csv",
  show_col_types = FALSE
)


#################################################################
#  COMBINE (1) results_df, (2) results_mini, (3) extra_details  #
#  Import extra cup game data scraped from 11v11                #
#################################################################

results_dataset <- results_df %>%
  dplyr::left_join(
    results_mini,
    by = "game_date"
  ) %>%
  dplyr::left_join(
    extra_details,
    by = "game_date"
  ) %>%
  dplyr::left_join(
    goalscorers_by_game,
    by = "game_date"
  ) %>%
  dplyr::mutate(
    ssn_year = as.numeric(stringr::str_sub(season, end = 4)),
    game_year = lubridate::year(game_date),
    game_month = lubridate::month(game_date),
    game_day = lubridate::day(game_date),
  )

usethis::use_data(results_dataset, overwrite = TRUE)
