#
# # Manually added sub mins
# cr_sub_mins <- vroom::vroom(
#   file = "https://raw.githubusercontent.com/petebrown/complete-record/main/output/cr_subs_and_reds.csv",
#   col_select = c("game_date", "player_name", "min_off", "min_on"),
#   na = "0",
#   show_col_types = FALSE
# )
#
# sb_sub_mins_96_to_99 <- sb_subs_and_reds %>%
#   dplyr::left_join(
#     sb_ids %>% dplyr::select(player_id, player_name) %>% unique(),
#     by = "player_id"
#   ) %>%
#   dplyr::left_join(
#     sb_ids %>% dplyr::select(game_id, game_date, season) %>% unique(),
#     by = "game_id"
#   ) %>%
#   dplyr::filter(
#     season %in% c("1996/97", "1997/98", "1998/99")
#   ) %>%
#   dplyr::select(
#     game_date,
#     player_name,
#     min_on,
#     min_off
#   ) %>%
#   dplyr::arrange(
#     game_date
#   )
#
# subs_pre_9900 <- rbind(
#   cr_sub_mins,
#   sb_sub_mins_96_to_99
# )
#
# cr_subs <- comp_rec_plr_apps %>%
#   dplyr::filter(
#     !is.na(on_for) | !is.na(off_for)
#   ) %>%
#   dplyr::select(
#     game_date,
#     shirt_no,
#     player_name,
#     on_for,
#     off_for
#   ) %>%
#   dplyr::group_by_all() %>%
#   dplyr::mutate(
#     shirt_nos = min(shirt_no, off_for, on_for, na.rm = TRUE)
#   ) %>%
#   dplyr::arrange(
#     game_date,
#     shirt_nos
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(
#     -shirt_nos
#   ) %>%
#   dplyr::left_join(
#     subs_pre_9900,
#     by = c("game_date", "player_name")
#   )
#
# sb_ids <- vroom::vroom(
#   file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
#   col_select = c("sb_game_id", "sb_player_id", "game_date", "season", "player_name"),
#   show_col_types = FALSE
# ) %>%
#   fix_sb_player_names() %>%
#   fix_sb_game_ids() %>%
#   dplyr::rename(
#     player_id = sb_player_id
#   )
#
# sb_subs <- sb_subs_and_reds %>%
#   dplyr::filter(
#     !is.na(min_on) | !is.na(min_off)
#   ) %>%
#   dplyr::left_join(
#     sb_ids,
#     by = c(
#       "game_id",
#       "player_id"
#     )
#   )  %>%
#   dplyr::left_join(
#     cr_subs,
#     by = c(
#       "game_date",
#       "player_name"
#     )
#   ) %>%
#   dplyr::select(
#     -shirt_no
#   ) %>%
#   dplyr::group_by_all() %>%
#   dplyr::mutate(
#     min = min(min_on, min_off, na.rm = TRUE)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(
#     game_date,
#     min,
#     min_off,
#     min_on
#   ) %>%
#   dplyr::left_join(
#     squad_nos,
#     by = c("season", "player_name"),
#     relationship = "many-to-many"
#   ) %>%
#   dplyr::mutate(
#     squad_no = dplyr::case_when(
#       season == "2014/15" & player_name == "Janoi Donacien" & game_date < "2015-03-07" ~ 19,
#       season == "2014/15" & player_name == "Janoi Donacien" & game_date >= "2015-03-07" ~ 12,
#       .default = squad_no
#     )
#   ) %>%
#   dplyr::rename(
#     shirt_no = squad_no
#   ) %>%
#   dplyr::select(
#     game_id,
#     game_date,
#     shirt_no,
#     player_id,
#     player_name,
#     min_on,
#     min_off
#   ) %>%
#   dplyr::filter(
#     !game_date %in% cr_subs$game_date
#   ) %>%
#   dplyr::select(
#     -game_id,
#     -player_id
#   )
#
# db_subs <- cr_subs %>%
#   dplyr::left_join(
#     sb_subs,
#     by = c("game_date", "shirt_no", "player_name")
#   ) %>%
#   dplyr::bind_rows(
#     sb_subs
#   ) %>%
#   dplyr::mutate(
#     min_off = dplyr::case_when(
#       game_date == "1997-01-10" & shirt_no == 6 ~ 72,
#       game_date == "1997-01-10" & shirt_no == 8 ~ 80,
#       game_date == "1997-01-10" & shirt_no == 10 ~ 81,
#       .default = min_off
#     ),
#     min_on = dplyr::case_when(
#       game_date == "1997-01-10" & shirt_no == 14 ~ 72,
#       game_date == "1997-01-10" & shirt_no == 13 ~ 80,
#       game_date == "1997-01-10" & shirt_no == 12 ~ 81,
#       .default = min_on
#     )
#   )

sub_mins <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/sub_mins.csv",
  show_col_types = FALSE
)

sub_plrs <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/subs.csv",
  show_col_types = FALSE
)

db_subs <- dplyr::left_join(
  sub_plrs,
  sub_mins,
  by = c(
    "game_date",
    "player_name"
  )
)

usethis::use_data(db_subs, overwrite = TRUE)
