meta_club_crests <- as.list(
  clubs_crests %>%
    dplyr::select(club, file_path) %>%
    dplyr::pull(file_path) %>%
    purrr::set_names(clubs_crests %>% dplyr::pull(club))
)

meta_plr_headshots <- as.list(
  player_imgs %>%
    dplyr::select(pl_index, headshot_file_path) %>%
    dplyr::pull(headshot_file_path) %>%
    purrr::set_names(player_imgs %>% dplyr::pull(pl_index))
)

meta_plr_positions <- as.list(
  player_positions %>%
    dplyr::select(pl_index, position) %>%
    dplyr::pull(position) %>%
    purrr::set_names(player_positions %>% dplyr::pull(pl_index))
)

# plr_debuts

# plr_dobs

meta_data <- list(
  'club_crests' = meta_club_crests,
  'plr_headshots' = meta_plr_headshots,
  'plr_positions' = meta_plr_positions
)


usethis::use_data(
  meta_plr_positions,
  meta_plr_headshots,
  meta_data,

  overwrite = TRUE
)
