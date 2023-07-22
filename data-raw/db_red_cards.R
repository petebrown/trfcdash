sb_ids <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/update-player-stats/main/data/players_df.csv",
  col_select = c("sb_game_id", "sb_player_id", "game_date", "season", "player_name"),
  show_col_types = FALSE
) %>%
  fix_sb_player_names() %>%
  fix_sb_game_ids() %>%
  dplyr::rename(player_id = sb_player_id)

red_cards_table <- sb_subs_and_reds %>%
  dplyr::filter(
    !is.na(min_so)
  ) %>%
  dplyr::left_join(
    sb_ids,
    by = c("game_id", "player_id")
  ) %>%
  dplyr::select(
    game_date,
    player_name,
    min_so
  )

usethis::use_data(red_cards_table, overwrite = TRUE)
