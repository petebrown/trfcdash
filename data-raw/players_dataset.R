## code to prepare `players_dataset` dataset goes here

fix_sb_player_names <- function(df) {
  df %>%
    dplyr::mutate(
      player_name = dplyr::case_when(
        player_name == "Corey Taylor" ~ "Corey Blackett-Taylor",
        player_name == "Craig Carl Curran" ~ "Craig Curran",
        player_name == "Chris Edwards" ~ "Christian Edwards",
        player_name == "Dave Nugent" ~ "David Nugent",
        player_name == "Dylan Mottley Henry" ~ "Dylan Mottley-Henry",
        player_name == "Ian Thomas-Moore" ~ "Ian Moore",
        player_name == "John-Louis Akpa Akpro" ~ "Jean-Louis Akpa Akpro",
        player_name == "Jonathon Margetts" ~ "Johnny Margetts",
        player_name == "Joseph Maguire" ~ "Joe Maguire",
        player_name == "Kaylden Brown" ~ "Kayleden Brown",
        player_name == "Lewis Sinnot" ~ "Lewis Sinnott",
        player_name == "Lateef Elford Alliyu" ~ "Lateef Elford-Alliyu",
        player_name == "Michael Jackson" ~ "Mike Jackson",
        player_name == "Richard Sutton" ~ "Ritchie Sutton",
        player_name == "Robert Taylor" ~ "Rob Taylor",
        player_name == "Samuel Taylor" ~ "Sam Taylor",
        player_name == "Steven O'Leary" ~ "Stephen O'Leary",
        # player_name == "Oliver Norburn" ~ "Ollie Norburn",
        # player_name == "Steven Jennings" ~ "Steven Jennings",
        .default = player_name
      )
    )
}

fix_sb_game_ids <- function(df) {
  df %>%
    dplyr::rename(game_id := sb_game_id) %>%
    dplyr::mutate(
      game_id := stringr::str_remove(game_id, "tpg"),
      game_id := as.numeric(game_id)
    )
}

comp_rec_plr_ssns <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/player_ssns.csv",
  show_col_types = FALSE
)


comp_rec_plr_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/apps_long.csv",
  show_col_types = FALSE)


sb_player_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
  col_select = c("sb_game_id", "sb_player_id", "game_date", "season", "player_name",  "pl_goals", "yellow_cards", "red_cards"),
  show_col_types = FALSE
) %>%
  fix_sb_player_names() %>%
  fix_sb_game_ids() %>%
  dplyr::rename(player_id = sb_player_id) %>%
  dplyr::select(
    player_id,
    player_name,
    game_id,
    game_date,
    season,
    pl_goals,
    yellow_cards,
    red_cards
  )


sb_subs_and_reds <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-events/main/data/subs-and-reds.csv",
  show_col_types = FALSE
)


comp_rec_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/cr-scorers.csv",
  show_col_types = FALSE
)

sb_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-goals/main/data/goals.csv",
  col_select = c("game_id", "player_id", "player_name", "minute", "penalty", "own_goal", "goal_type"),
  show_col_types = FALSE
) %>%
  fix_sb_player_names()

goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/scorers-long.csv",
  show_col_types = FALSE
)

usethis::use_data(players_dataset, overwrite = TRUE)
