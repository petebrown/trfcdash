############
# MANAGERS #
############

managers <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/managers.csv",
  show_col_types = FALSE
) %>%
  dplyr::rename(
    mgr_role = role
  ) %>%
  dplyr::arrange(
    date_from
  ) %>%
  dplyr::group_by(
    manager_name
  ) %>%
  dplyr::mutate(
    mgr_spell_no = dplyr::row_number()
  ) %>%
  dplyr::ungroup()

usethis::use_data(
  managers,
  overwrite = TRUE
)


###########
# RESULTS #
###########

results_dataset <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/results.csv",
  show_col_types = FALSE
)

first_game_current_season <- min(results_dataset[results_dataset$season == max(results_dataset$season), ]$game_date)

#########
# GOALS #
#########

goals_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/goals.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    game_date < first_game_current_season
  )


goals <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/goals.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    game_date >= first_game_current_season
  )


goals <- dplyr::bind_rows(
  goals_pre_23,
  goals
)


player_goals_per_game <- goals %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      .default = player_name,
      own_goal > 0 & player_name != "OG" ~ stringr::str_glue("{player_name} (OG)")
    )
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    pens = sum(penalty, na.rm = TRUE),
    .groups = "drop"
  )


goalscorers_by_game <- goals %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    pens = sum(penalty, na.rm = TRUE),
    own_goals = sum(own_goal, na.rm = TRUE),
    min_goal_min = min(goal_min),
    .groups = "drop"
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::mutate(
    player_name = dplyr::case_when(
      goals_scored == 1 & pens == 0 & own_goals == 0 ~ player_name,
      goals_scored > 1 & pens == 0 & own_goals == 0 ~ stringr::str_glue("{player_name} {goals_scored}"),
      goals_scored == 1 & pens == 1 & own_goals == 0 ~ stringr::str_glue("{player_name} (pen)"),
      goals_scored > 1 & pens == 1 & own_goals == 0 ~ stringr::str_glue("{player_name} {goals_scored} (1 pen)"),
      goals_scored > 1 & pens > 1 & own_goals == 0 ~ stringr::str_glue("{player_name} {goals_scored} ({pens} pens)"),
      own_goals > 0 & player_name != "OG" ~ stringr::str_glue("{player_name} (OG)"),
      .default = player_name
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    game_date,
    min_goal_min
  ) %>%
  dplyr::group_by(
    game_date
  ) %>%
  dplyr::summarise(
    scorers = paste(player_name, collapse = ", ")
  ) %>%
  dplyr::select(
    game_date,
    scorers
  )


goals_by_game_with_mins <- goals %>%
  dplyr::filter(
    !is.na(goal_min)
  ) %>%
  dplyr::mutate(
    goal_text = dplyr::case_when(
      penalty == 1 ~ paste0(goal_min, " pen"),
      own_goal == 1 & player_name != "OG" ~ paste0(goal_min, " og"),
      .default = as.character(goal_min)
    )
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::reframe(
    min_goal = min(goal_min),
    goal_text = paste0(goal_text, collapse = ", ")
  ) %>%
  dplyr::ungroup() %>%
  unique() %>%
  dplyr::arrange(
    game_date,
    min_goal
  ) %>%
  dplyr::mutate(
    goal_text = paste0(player_name, " (", goal_text, ")")
  ) %>%
  dplyr::select(
    game_date,
    goal_text
  ) %>%
  dplyr::group_by(
    game_date
  ) %>%
  dplyr::summarise(
    text = paste(goal_text, collapse = ", "),
    html = paste0("<p>", paste0(goal_text, collapse = "<br>"), "</p>"),
    .groups = "drop"
  )

goals_by_game_without_mins <- goals %>%
  dplyr::filter(
    is.na(goal_min)
  ) %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::reframe(
    n_goals = dplyr::n(),
    goal_text = dplyr::case_when(
      n_goals > 1 ~ paste0(player_name, " (", n_goals, ")"),
      .default = player_name
    )
  ) %>%
  dplyr::ungroup() %>%
  unique() %>%
  dplyr::select(
    game_date,
    goal_text
  ) %>%
  dplyr::group_by(
    game_date
  ) %>%
  dplyr::summarise(
    text = paste(goal_text, collapse = ", "),
    html = paste0("<p>", paste0(goal_text, collapse = "<br>"), "</p>"),
    .groups = "drop"
  )


goals_with_info <- dplyr::bind_rows(
  goals_by_game_with_mins,
  goals_by_game_without_mins
) %>%
  dplyr::arrange(
    game_date
  )

usethis::use_data(
  goals,
  player_goals_per_game,
  goalscorers_by_game,
  goals_with_info,

  overwrite = TRUE
)

###########
# RESULTS #
###########

results_dataset <- results_dataset %>%
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


results_dataset <- results_dataset %>%
  dplyr::left_join(
    managers,
    dplyr::join_by(
      "manager" == "manager_name",
      "game_date" >= "date_from",
      "game_date" <= "date_to"
    )
  ) %>%
  dplyr::mutate(
    mgr_role = dplyr::case_when(
      manager %in% c("Kevin Sheedy & Ray Mathias", "Jason McAteer & John McMahon") ~ "Caretaker",
      TRUE ~ mgr_role
    )
  ) %>%
  dplyr::select(
    -date_from,
    -date_to
  )


first_game_current_season <- min(results_dataset[results_dataset$season == max(results_dataset$season), ]$game_date)

game_lengths <- results_dataset %>%
  dplyr::select(
    game_date,
    game_length
  )


season_game_dates <- results_dataset %>%
  dplyr::select(
    game_date,
    season
  )


season_game_nos <- results_dataset %>%
  dplyr::select(
    game_date,
    game_no
  )


usethis::use_data(
  results_dataset,
  game_lengths,
  season_game_dates,
  season_game_nos,

  overwrite = TRUE
)


#############
# RED CARDS #
#############

rc_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/red_cards.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    red_cards = dplyr::n()
  ) %>%
  dplyr::filter(
    game_date < first_game_current_season
  )


red_cards <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/red_cards.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    red_cards = dplyr::n()
  ) %>%
  dplyr::filter(
    game_date >= first_game_current_season
  )


red_cards <- dplyr::bind_rows(
  rc_pre_23,
  red_cards
)


usethis::use_data(
  red_cards,
  overwrite = TRUE
)


########
# SUBS #
########

sub_mins_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/sub_mins.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    game_date < first_game_current_season
  )


sub_mins <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/sub_mins.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    game_date >= first_game_current_season
  )


sub_mins <- dplyr::bind_rows(
  sub_mins_pre_23,
  sub_mins
)


sub_plrs_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/subs.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by(
    game_date,
    shirt_no,
    player_name
  ) %>%
  dplyr::summarise(
    on_for = sum(on_for, na.rm = TRUE),
    off_for = sum(off_for, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    on_for = dplyr::na_if(on_for, 0),
    off_for = dplyr::na_if(off_for, 0)
  ) %>%
  dplyr::filter(
    game_date < first_game_current_season
  )


sub_plrs <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/subs.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by(
    game_date,
    shirt_no,
    player_name
  ) %>%
  dplyr::summarise(
    on_for = sum(on_for, na.rm = TRUE),
    off_for = sum(off_for, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    on_for = dplyr::na_if(on_for, 0),
    off_for = dplyr::na_if(off_for, 0)
  ) %>%
  dplyr::filter(
    game_date >= first_game_current_season
  )


sub_plrs <- dplyr::bind_rows(
  sub_plrs_pre_23,
  sub_plrs
)


subs <- dplyr::full_join(
  sub_plrs,
  sub_mins,
  by = c(
    "game_date",
    "player_name"
  )
) %>%
  dplyr::select(
    -shirt_no
  )


usethis::use_data(
  subs,
  overwrite = TRUE
)


################
# YELLOW_CARDS #
################

yc_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/yellow_cards.csv",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    yellow_cards = dplyr::n()
  )


yellow_cards <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/yellow_cards.csv",
  col_select = -"min_yc",
  show_col_types = FALSE
) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    yellow_cards = dplyr::n()
  )

usethis::use_data(
  yellow_cards,
  overwrite = TRUE
)


###########
# PLAYERS #
###########

player_apps_pre_23 <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/player_apps.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    game_date < first_game_current_season
  )


player_apps <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/player_apps.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    game_date >= first_game_current_season
  )


player_apps <- dplyr::bind_rows(
  player_apps_pre_23,
  player_apps
)


player_info <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/pre-2023-data-prep/main/data/player_dobs.csv",
  show_col_types = FALSE
)

player_pos <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/wrangle-player-positions/main/output/player-positions.csv",
  show_col_types = FALSE
)

player_positions <- player_info %>% dplyr::select(pl_index, player_name, player_dob) %>% dplyr::distinct() %>%
  dplyr::left_join(
    player_pos %>% dplyr::select(-surname, -forename),
    by = c('player_name', 'player_dob')
  ) %>%
  dplyr::mutate(
    position = dplyr::case_when(
      !is.na(comp_rec_pos) ~ comp_rec_pos,
      !is.na(tm_pos_1) ~ tm_pos_1,
      !is.na(soccerbase_pos) ~ soccerbase_pos,
      TRUE ~ "Unknown"
    )
  )

player_ids <- player_positions %>%
  dplyr::select(
    pl_index,
    player_id
  )


player_goals_per_game <- goals %>%
  dplyr::group_by(
    game_date,
    player_name
  ) %>%
  dplyr::summarise(
    goals_scored = dplyr::n(),
    .groups = "drop"
  )


player_apps <- player_apps %>%
  dplyr::left_join(
    season_game_nos,
    by = "game_date"
  ) %>%
  dplyr::left_join(
    season_game_dates,
    by = "game_date"
  ) %>%
  dplyr::left_join(
    player_goals_per_game,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::left_join(
    yellow_cards,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::left_join(
    red_cards,
    by = c(
      "game_date",
      "player_name"
    )
  )  %>%
  dplyr::left_join(
    subs,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::left_join(
    player_info,
    by = c(
      "player_name",
      "season"
    )
  ) %>%
  tidyr::replace_na(
    list(
      goals_scored = 0,
      yellow_cards = 0,
      red_cards = 0
    )
  ) %>%
  dplyr::left_join(
    game_lengths,
    by = "game_date"
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    mins_played = dplyr::case_when(
      # Started, played to end
      role == "starter" & is.na(min_off) & is.na(off_for) & is.na(min_so) ~ game_length,
      # Started, subbed off
      role == "starter" & !is.na(min_off) & is.na(min_so) ~ min_off,
      # Started, sent off
      role == "starter" & is.na(min_off) & !is.na(min_so) ~ min_so,
      # Subbed on, played to end
      role == "sub" & is.na(min_off) & is.na(off_for) & is.na(min_so) ~ game_length - min_on,
      # Subbed on, subbed off
      role == "sub" & !is.na(min_off) & is.na(min_so) ~ min_off - min_on,
      # Subbed on, sent off
      role == "sub" & is.na(min_off) & !is.na(min_so) ~ min_so - min_on,
    )
  ) %>% dplyr::ungroup() %>%
  dplyr::rename(
    menu_name = pl_index
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
    min_on,
    min_off,
    player_dob,
    dob_display,
    menu_name
  )

player_debuts <- player_apps %>%
  dplyr::arrange(
    game_date
  ) %>%
  dplyr::group_by(
    menu_name
  ) %>%
  dplyr::filter(
    dplyr::row_number() == 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    menu_name,
    player_name,
    player_dob,
    game_date
  ) %>%
  dplyr::mutate(
    plr_game_age = game_date - player_dob,
    age_yrs = floor(as.numeric(plr_game_age / 365.25)),
    age_days = ceiling(plr_game_age - (age_yrs * 365.25)),
    plr_game_age = as.integer(plr_game_age)
  ) %>%
  dplyr::rename(
    debut_date = game_date
  )

usethis::use_data(
  player_apps,
  player_info,
  player_ids,
  player_positions,
  player_debuts,

  overwrite = TRUE
)


##############
# LGE_TABLES #
##############

non_league_tabs <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-national-league-results-and-tables/main/national_league_tabs.csv",
  show_col_types = FALSE
) %>%
  dplyr::left_join(
    results_dataset %>% dplyr::select(game_date, season, ssn_comp_game_no) %>% dplyr::distinct(),
    by = "game_date"
  ) %>%
  dplyr::select(
    season,
    game_no = ssn_comp_game_no,
    game_date,
    pos,
    Team,
    Pld,
    W,
    D,
    L,
    GF,
    GA,
    GD,
    Pts
  )


lge_tables <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/lge_tables.csv",
  show_col_types = FALSE,
  col_select = -("url")
) %>%
  dplyr::mutate(
    GD = GF - GA
  ) %>%
  dplyr::relocate(
    GD,
    .after = GA
  ) %>%
  dplyr::bind_rows(
    non_league_tabs
  ) %>%
  dplyr::arrange(
    season,
    game_no,
    pos
  )


lge_tables_by_venue <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/lge_tables_venue.csv",
  show_col_types = FALSE,
  col_select = -("url")
)


lge_tables_eos <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/scrape-latest-table/main/data/eos_tables.csv",
  show_col_types = FALSE,
  col_select = -("url")
)


usethis::use_data(
  lge_tables,
  lge_tables_eos,
  lge_tables_by_venue,

  overwrite = TRUE
)


##############################
##     UPDATE COMPLETE      ##
##############################

