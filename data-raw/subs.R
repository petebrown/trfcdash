sub_mins <- vroom::vroom(
  file = "https://raw.githubusercontent.com/petebrown/data-updater/main/data/sub_mins.csv",
  show_col_types = FALSE
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
