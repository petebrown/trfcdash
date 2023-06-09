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
        player_name == "John Morrissey" ~ "Johnny Morrissey",
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
) %>%
  fix_sb_player_names()


game_ids_and_dates <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
  col_select = c("sb_game_id", "game_date", "season"),
  show_col_types = FALSE
) %>%
  fix_sb_game_ids() %>%
  unique()

game_dates_and_nos <- results_dataset %>%
  dplyr::select(
    season,
    ssn_game_no,
    game_date
  )




goalscorers_by_game <- sb_goals %>%
  fix_sb_player_names() %>%
  dplyr::filter(
    goal_type == "for"
  ) %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      own_goal == 1 ~ "OG",
      .default = player_name
    )
  ) %>%
  dplyr::left_join(
    game_ids_and_dates,
    by = "game_id"
  ) %>%
  dplyr::select(
    season,
    game_date,
    player_name,
    minute,
    penalty
  ) %>%
  dplyr::arrange(
    game_date,
    minute
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    pens = sum(penalty),
    .groups = "drop"
  ) %>%
  dplyr::bind_rows(
    comp_rec_goals
  ) %>%
  dplyr::select(
    game_date,
    player_name,
    goals_scored,
    pens
  ) %>%
  dplyr::group_by(game_date) %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      goals_scored == 1 & pens == 0 ~ player_name,
      goals_scored > 1 & pens == 0 ~ stringr::str_glue("{player_name} {goals_scored}"),
      goals_scored == 1 & pens == 1 ~ stringr::str_glue("{player_name} (pen)"),
      goals_scored > 1 & pens == 1 ~ stringr::str_glue("{player_name} {goals_scored} (1 pen)"),
      goals_scored > 1 & pens > 1 ~ stringr::str_glue("{player_name} {goals_scored} ({pens} pens)"),
      .default = stringr::str_glue("{player_name}"),
    ),
    scorers = paste(player_name, collapse = ", ")
  ) %>%
  dplyr::select(
    game_date,
    scorers
  ) %>%
  unique() %>%
  dplyr::arrange(
    game_date
  )


game_lengths <- results_dataset %>%
  dplyr::mutate(
    game_length = dplyr::case_when(
      !is.na(gg_outcome) ~ 116,
      is.na(gg_outcome) & extra_time == 1 ~ 120,
      .default = 90
    )
  ) %>%
  dplyr::select(
    game_date,
    game_length
  )




comp_rec_plr_seasons <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/player_ssns.csv",
  show_col_types = FALSE
)

comp_rec_plr_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/apps_long.csv",
  show_col_types = FALSE) %>%
  dplyr::left_join(
    game_dates_and_nos,
    by = c("season" = "season", "game_no" = "ssn_game_no")
  ) %>%
  dplyr::left_join(
    comp_rec_plr_seasons %>%
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
    goals,
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
  )# %>%
  # dplyr::left_join(
  #   game_lengths,
  #   by = "game_date"
  # ) %>%
  # dplyr::mutate(
  #   mins_played = dplyr::case_when(
  #     role == "starter" & is.na(off_for) ~ game_length,
  #     .default = NA
  #   )
  # ) %>%
  # dplyr::select(
  #   -game_length
  # )



squad_nos <- vroom::vroom(
  "https://raw.githubusercontent.com/petebrown/complete-record/main/squad_nos/squad_nos.csv",
  show_col_types = FALSE
)

sb_subs_and_reds <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-events/main/data/subs-and-reds.csv",
  show_col_types = FALSE
)

sb_player_dob <- vroom::vroom(
  "https://raw.githubusercontent.com/petebrown/scrape-player-info/main/data/player-info.csv",
  show_col_types = FALSE
) %>%
  dplyr::select(
    player_id,
    player_dob
  )

sb_player_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
  col_select = c("sb_game_id", "sb_player_id", "game_date", "season", "player_name",  "pl_goals", "yellow_cards", "red_cards"),
  show_col_types = FALSE
) %>%
  fix_sb_player_names() %>%
  fix_sb_game_ids() %>%
  dplyr::rename(player_id = sb_player_id) %>%
  dplyr::left_join(
    sb_player_dob,
    by = "player_id"
  ) %>%
  dplyr::select(
    player_id,
    player_name,
    player_dob,
    game_id,
    game_date,
    season,
    pl_goals,
    yellow_cards,
    red_cards
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
  unique() %>%
  dplyr::left_join(
    y = sb_subs_and_reds,
    by = c("game_id", "player_id")
  ) %>%
  dplyr::left_join(
    game_lengths,
    by = "game_date"
  ) %>%
  dplyr::mutate(
    role = dplyr::case_when(
      is.na(min_on) ~ "starter",
      !is.na(min_on) ~ "sub"
    ),
    mins_played = dplyr::case_when(
      role == "starter" & is.na(min_off) & is.na(min_so) ~ game_length,
      role == "starter" & !is.na(min_off) ~ min_off,
      role == "starter" & !is.na(min_so) ~ min_so,
      role == "sub" & is.na(min_off) & is.na(min_so) ~ game_length - min_on,
      role == "sub" & is.na(min_off) ~ min_off - min_on,
      role == "sub" & is.na(min_so) ~ min_so - min_on
    )
  ) %>%
  dplyr::select(
    season,
    game_date,
    player_name,
    player_dob,
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



player_apps <- dplyr::bind_rows(
  comp_rec_plr_apps,
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

cr_pl_dobs <- vroom::vroom(
  "https://raw.githubusercontent.com/petebrown/complete-record/main/output/player_stats.csv",
  show_col_types = FALSE
) %>%
  dplyr::mutate(
    player_name = paste(forename, surname),
    pl_name_index = paste(surname, forename, sep = ", ")
  ) %>%
  dplyr::rename(
    player_dob = dob
  ) %>%
  dplyr::group_by(
    player_name,
    player_dob
  ) %>%
  dplyr::mutate(
    min_season = min(ssn_join),
    max_season = max(ssn_lve)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dob_display = dplyr::case_when(
      !is.na(player_dob) ~ NA,
      is.na(player_dob) & !is.na(dob_qtr) & !is.na(dob_yr) ~ stringr::str_glue("Q{dob_qtr} {dob_yr}"),
      is.na(player_dob) & is.na(dob_qtr) & !is.na(dob_yr) & dob_yr_is_est == 0 ~ as.character(dob_yr),
      is.na(player_dob) & is.na(dob_qtr) & !is.na(dob_yr) & dob_yr_is_est == 1 ~ paste0("~", dob_yr),
      is.na(player_dob) & is.na(dob_qtr) & is.na(dob_yr) & dob_yr_is_est == 0 ~ "Unknown"
    )
  ) %>%
  dplyr::select(
    surname,
    forename,
    player_name,
    pl_name_index,
    player_dob,
    dob_display,
    min_season,
    max_season
  ) %>%
  dplyr::distinct()

sb_pl_dobs <- sb_player_apps %>%
  dplyr::group_by(
    player_name,
    player_dob
  ) %>%
  dplyr::summarise(
    min_season = min(season),
    max_season = max(season),
    min_season = stringr::str_split_i(min_season, "/", 1),
    max_season = stringr::str_split_i(max_season, "/", 1),
    min_season = as.numeric(min_season),
    max_season = as.numeric(max_season)
  ) %>%
  dplyr::mutate(
    forename = dplyr::case_match(
      player_name,
      "Pedro Miguel Matias" ~ "Pedro Miguel",
      .default = stringr::str_split_i(player_name, " ", 1)
    ),
    surname = dplyr::case_match(
      player_name,
      "Jean-Louis Akpa Akpro" ~ "Akpa Akpro",
      "Owain Fon Williams" ~ "Fon Williams",
      "Craig Le Cornu" ~ "Le Cornu",
      "Ian St John" ~ "St John",
      .default = stringr::str_split_i(player_name, " ", 2)
    ),
    pl_name_index = paste(surname, forename, sep = ", ")
  ) %>%
  dplyr::select(
    surname,
    forename,
    player_name,
    pl_name_index,
    player_dob,
    min_season,
    max_season
  ) %>%
  dplyr::arrange(
    surname,
    forename,
    player_name,
    player_dob
  )

player_info <- dplyr::bind_rows(
  cr_pl_dobs, sb_pl_dobs
) %>%
  dplyr::group_by(
    player_name,
    player_dob
  ) %>%
  dplyr::slice(
    rep(1:dplyr::n(), each = max_season - min_season + 1)
  ) %>%
  dplyr::mutate(
    season = min_season + dplyr::row_number() - 1,
    nxt_season = as.character(season + 1),
    nxt_season = stringr::str_sub(nxt_season, start = 3, end = 4),
    season = stringr::str_glue("{season}/{nxt_season}")
  ) %>%
  dplyr::select(
    -min_season,
    -max_season,
    -nxt_season
  )

dupe_names <- player_info %>%
  dplyr::arrange(
    surname,
    forename,
    season
  ) %>%
  dplyr::select(
    pl_name_index,
    player_dob
  ) %>%
  unique() %>%
  dplyr::group_by(
    pl_name_index
  ) %>%
  dplyr::summarise(
    count = dplyr::n()
  ) %>%
  dplyr::filter(
    count > 1
  )

menu_names <- player_info %>%
  dplyr::mutate(
    disam_text = dplyr::case_when(
      !is.na(player_dob) ~ as.character(lubridate::year(player_dob)),
      dob_display != "Unknown" ~ stringr::str_sub(dob_display, -4),
      dob_display != "Unknown" ~ "?"
    ),
    menu_name = dplyr::case_when(
      pl_name_index %in% dupe_names$pl_name_index ~ stringr::str_glue("{pl_name_index} (b.{disam_text})"),
      .default = pl_name_index
    )
  ) %>%
  dplyr::select(
    player_name,
    season,
    player_dob,
    dob_display,
    menu_name
  )

player_apps <- player_apps %>%
  dplyr::left_join(
    menu_names,
    by = c("player_name", "season")
  )

usethis::use_data(player_apps, goals, goalscorers_by_game, player_info, overwrite = TRUE)
