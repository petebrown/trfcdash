#' utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
import_cr_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/apps_long.csv",
  col_select = c("sb_game_id", "sb_player_id", "game_date", "season", "player_name",  "pl_goals", "yellow_cards", "red_cards"),
  show_col_types = FALSE)

import_cr_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/cr-scorers.csv",
  show_col_types = FALSE
)

import_cr_player_ssns <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/player_ssns.csv",
  show_col_types = FALSE
)

import_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/scorers-long.csv",
  show_col_types = FALSE
)

import_results_mini <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/league-position-tool/main/docs/input/results_mini.csv",
  col_select = c("game_date", "ranking", "pts"),
  show_col_types = FALSE)

import_results <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-results/main/data/results_df.csv",
  col_select = c("game_date", "season", "opposition", "venue", "home_team", "away_team", "score", "secondary_score", "outcome", "competition", "goals_for", "goals_against", "attendance", "league_tier", "generic_comp", "game_type", "ssn_game_no", "ssn_comp_game_no", "weekday", "manager"),
  show_col_types = FALSE)

import_sb_goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-goals/main/data/goals.csv",
  col_select = c("game_id", "player_id", "player_name", "minute", "penalty", "own_goal", "goal_type"),
  show_col_types = FALSE
)

import_sb_player_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
  show_col_types = FALSE
)

import_sb_subs_and_reds <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-events/main/data/subs-and-reds.csv",
  show_col_types = FALSE
)

import_managers <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-managers/main/data/managers_df.csv",
  show_col_types = FALSE
)

import_squad_nos <- vroom::vroom(
  "https://raw.githubusercontent.com/petebrown/complete-record/main/squad_nos/squad_nos.csv",
  show_col_types = FALSE
)

fix_sb_player_names <- function(df) {
  df <- df %>%
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
  return (df)
}

# Clean, join, wrangle imported datasets
get_results_df <- function() {
  df <- dplyr::full_join(
    x = import_results,
    y = import_results_mini,
    by = "game_date"
  ) %>%
    dplyr::rename(
      league_pos = ranking
    )
  return (df)
}

get_cr_player_seasons <- function() {
  df <- import_cr_player_ssns
  return(df)
}

get_sb_player_apps <- function() {
  df <- import_sb_player_apps %>%
    dplyr::rename(
      game_id = sb_game_id,
      player_id = sb_player_id,
    ) %>%
    dplyr::mutate(
      game_id = stringr::str_remove(game_id, "tpg"),
      game_id = as.numeric(game_id)
    ) %>%
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

  return (df)
}

get_goals_df <- function() {
  df <- import_goals %>%
    dplyr::group_by(
      season,
      game_no,
      player_name
    ) %>%
    dplyr::summarise(
      goals_scored = sum(goals_scored),
      .groups = "drop"
    )
  return (df)
}

get_managers_df <- function() {
  df <- import_managers %>%
    dplyr::rename(manager_code = manager_sb_code)
  return (df)
}

get_squad_nos <- function() {
  df <- import_squad_nos

  return(df)
}


# Basic data manipulation functions used across dashboard
filter_ssn_results <- function(selected_seasons) {
  results_df <- get_results_df() %>%
    dplyr::filter(season %in% selected_seasons)

  return (results_df)
}


get_player_apps_df <- function() {
  results_df <- get_results_df() %>%
    dplyr::select(
      season,
      game_date,
      ssn_game_no
    ) %>%
    dplyr::rename(game_no = ssn_game_no)

  goals_df <- get_goals_df()

  cr_player_apps <- dplyr::left_join(
    x = import_cr_apps,
    y = results_df,
    by = c("season", "game_no")
  )

  cr_player_ssns <- get_cr_player_seasons()

  sb_player_apps <- get_sb_player_apps()
  sb_subs_and_reds <- import_sb_subs_and_reds
  squad_nos <- get_squad_nos_long()

  sb_player_apps <- dplyr::full_join(
    x = sb_player_apps,
    y = sb_subs_and_reds,
    by = c("game_id", "player_id")
  ) %>%
    dplyr::mutate(
      role = dplyr::case_when(
        is.na(min_on) ~ "starter",
        !is.na(min_on) ~ "sub"
      ),
      mins_played = dplyr::case_when(
        role == "starter" & is.na(min_off) & is.na(min_so) ~ 90,
        role == "starter" & !is.na(min_off) ~ min_off,
        role == "starter" & !is.na(min_so) ~ min_so,
        role == "sub" & is.na(min_off) & is.na(min_so) ~ 90 - min_on,
        role == "sub" & is.na(min_off) ~ min_off - min_on,
        role == "sub" & is.na(min_so) ~ min_so - min_on
      )
    ) %>%
    dplyr::left_join(
      results_df,
      by = c("game_date", "season")
    ) %>%
    dplyr::left_join(
      squad_nos,
      by = c("season", "game_date", "player_name")
    )%>%
    dplyr::select(
      season,
      game_no,
      game_date,
      player_name,
      role,
      pl_goals,
      squad_no,
      mins_played,
      yellow_cards,
      red_cards
    ) %>%
    dplyr::rename(
      goals_scored = pl_goals,
      shirt_no = squad_no
    )

  cr_player_apps <- cr_player_apps %>%
    dplyr::left_join(
      cr_player_ssns %>%
        dplyr::select(
          season,
          disam_name,
          player_name
        ) %>% unique(),
      by = c("season" = "season", "player_name" = "disam_name")
    ) %>%
    dplyr::mutate(
      player_name = player_name.y
    ) %>%
    dplyr::select(-player_name.y) %>%
    dplyr::left_join(
      goals_df,
      by = c("season" = "season", "game_no" = "game_no", "player_name" = "player_name")
    ) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      player_name,
      role,
      goals_scored,
      shirt_no,
      on_for,
      off_for
    )

  df <- dplyr::bind_rows(
    cr_player_apps,
    sb_player_apps
  ) %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      player_name,
      role,
      goals_scored,
      mins_played,
      yellow_cards,
      red_cards,
      shirt_no,
      on_for,
      off_for,
    )

  return (df)
}

get_squad_nos_long <- function() {
  player_apps <- get_sb_player_apps() %>%
    fix_sb_player_names()
  squad_nos <- get_squad_nos()

  df <- player_apps %>%
    dplyr::select(
      season,
      game_date,
      player_name
    ) %>%
    dplyr::left_join(
      squad_nos,
      by = c("season", "player_name"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      squad_no = dplyr::case_when(
        season == "2014/15" & player_name == "Janoi Donacien" & game_date < "2015-03-07" ~ 19,
        season == "2014/15" & player_name == "Janoi Donacien" & game_date >= "2015-03-07" ~ 12,
        .default = squad_no
      )
    ) %>%
    unique()

  return(df)
}










