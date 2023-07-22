cr_subs <- comp_rec_plr_apps %>%
  dplyr::filter(
    !is.na(on_for) | !is.na(off_for)
  ) %>%
  dplyr::select(
    game_date,
    shirt_no,
    player_name,
    off_for,
    on_for
  ) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    shirt_nos = min(shirt_no, off_for, on_for, na.rm = TRUE)
  ) %>%
  dplyr::arrange(
    game_date,
    shirt_nos
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    -shirt_nos
  )

sb_ids <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
  col_select = c("sb_game_id", "sb_player_id", "game_date", "season", "player_name"),
  show_col_types = FALSE
) %>%
  fix_sb_player_names() %>%
  fix_sb_game_ids() %>%
  dplyr::rename(player_id = sb_player_id)

sb_subs <- sb_subs_and_reds %>%
  dplyr::filter(
    !is.na(min_on) | !is.na(min_off)
  ) %>%
  dplyr::left_join(
    sb_ids,
    by = c(
      "game_id",
      "player_id"
    )
  )  %>%
  dplyr::left_join(
    cr_subs,
    by = c(
      "game_date",
      "player_name"
    )
  ) %>%
  dplyr::select(
    -shirt_no
  ) %>%
  dplyr::group_by_all() %>%
  dplyr::mutate(
    min = min(min_on, min_off, na.rm = TRUE)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(
    game_date,
    min,
    min_off,
    min_on
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
  dplyr::rename(
    shirt_no = squad_no
  ) %>%
  dplyr::select(
    game_id,
    game_date,
    shirt_no,
    player_id,
    player_name,
    min_off,
    min_on
  ) %>%
  dplyr::filter(
    !game_date %in% cr_subs$game_date
  ) %>%
  dplyr::select(
    -game_id,
    -player_id
  )

subs_table <- cr_subs %>%
  dplyr::left_join(
    sb_subs,
    by = c("game_date", "shirt_no", "player_name")
  ) %>%
  dplyr::bind_rows(
    sb_subs
  ) %>%
  dplyr::mutate(
    min_off = dplyr::case_match(
      shirt_no,
      6 ~ 72,
      8 ~ 80,
      10 ~ 81,
      .default = min_off
    ),
    min_on = dplyr::case_match(
      shirt_no,
      14 ~ 72,
      13 ~ 80,
      12 ~ 81,
      .default = min_on
    )
  )

usethis::use_data(subs_table, overwrite = TRUE)
