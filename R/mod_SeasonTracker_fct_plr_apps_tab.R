# selected_season <- "2023/24"
#
# df <- player_apps %>%
#   dplyr::filter(
#     season %in% selected_season
#   ) %>%
#   dplyr::left_join(
#     results_dataset,
#     by = c(
#       "season",
#       "game_date",
#       "game_no"
#     )
#   ) %>%
#   dplyr::mutate(
#     generic_comp = dplyr::case_when(
#       game_type == "League" ~ "League",
#       game_type == "Cup" & !generic_comp %in% c("FA Cup", "League Cup") ~ "Other",
#       TRUE ~ generic_comp
#     )
#   ) %>%
#   dplyr::group_by(
#     season,
#     menu_name,
#     generic_comp
#   ) %>%
#   dplyr::summarise(
#     apps = dplyr::n(),
#     starts = sum(role == "starter"),
#     sub_apps = sum(role == "sub"),
#     goals = sum(goals_scored),
#     yellows = sum(yellow_cards),
#     reds = sum(red_cards),
#     mins_played = sum(mins_played),
#     .groups = "drop"
#   ) %>%
#   # Long to wide
#   tidyr::pivot_wider(
#     id_cols = c(
#       season,
#       menu_name
#     ),
#     names_from = generic_comp,
#     values_from = c(
#       apps,
#       starts,
#       sub_apps,
#       goals,
#       yellows,
#       reds,
#       mins_played
#     ),
#     names_sep = "_",
#     values_fill = 0
#   ) %>%
#   dplyr::select(
#     season,
#     menu_name,
#     dplyr::ends_with("League"),
#     dplyr::ends_with("FA Cup"),
#     dplyr::ends_with("League Cup"),
#     dplyr::ends_with("Other"),
#   )
#
# reactable::reactable(
#   data = df,
#   columns = list(
#     "season" = reactable::colDef(
#       name = "Season"
#     ),
#     "menu_name" = reactable::colDef(
#       name = "Player"
#     ),
#     "apps_League" = reactable::colDef(
#       name = "Total Apps"
#     ),
#     "starts_League" = reactable::colDef(
#       name = "Starts"
#     ),
#     "sub_apps_League" = reactable::colDef(
#       name = "Sub Apps"
#     ),
#     "goals_League" = reactable::colDef(
#       name = "Goals"
#     ),
#     "yellows_League" = reactable::colDef(
#       name = "Yellows"
#     ),
#     "reds_League" = reactable::colDef(
#       name = "Reds"
#     ),
#     "mins_played_League" = reactable::colDef(
#       name = "Mins Played"
#     ),
#     "apps_FA Cup" = reactable::colDef(
#       name = "Total Apps"
#     ),
#     "starts_FA Cup" = reactable::colDef(
#       name = "Starts"
#     ),
#     "sub_apps_FA Cup" = reactable::colDef(
#       name = "Sub Apps"
#     ),
#     "goals_FA Cup" = reactable::colDef(
#       name = "Goals"
#     ),
#     "yellows_FA Cup" = reactable::colDef(
#       name = "Yellows"
#     ),
#     "reds_FA Cup" = reactable::colDef(
#       name = "Reds"
#     ),
#     "mins_played_FA Cup" = reactable::colDef(
#       name = "Mins Played"
#     ),
#     "apps_League Cup" = reactable::colDef(
#       name = "Total Apps"
#     ),
#     "starts_League Cup" = reactable::colDef(
#       name = "Starts"
#     ),
#     "sub_apps_League Cup" = reactable::colDef(
#       name = "Sub Apps"
#     ),
#     "goals_League Cup" = reactable::colDef(
#       name = "Goals"
#     ),
#     "yellows_League Cup" = reactable::colDef(
#       name = "Yellows"
#     ),
#     "reds_League Cup" = reactable::colDef(
#       name = "Reds"
#     ),
#     "mins_played_League Cup" = reactable::colDef(
#       name = "Mins Played"
#     ),
#     "apps_Other" = reactable::colDef(
#       name = "Total Apps"
#     ),
#     "starts_Other" = reactable::colDef(
#       name = "Starts"
#     ),
#     "sub_apps_Other" = reactable::colDef(
#       name = "Sub Apps"
#     ),
#     "goals_Other" = reactable::colDef(
#       name = "Goals"
#     ),
#     "yellows_Other" = reactable::colDef(
#       name = "Yellows"
#     ),
#     "reds_Other" = reactable::colDef(
#       name = "Reds"
#     ),
#     "mins_played_Other" = reactable::colDef(
#       name = "Mins Played"
#     )
#   ),
#   columnGroups = list(
#     reactable::colGroup(
#       name = "League",
#       columns = c(
#         "apps_League",
#         "starts_League",
#         "sub_apps_League",
#         "goals_League",
#         "yellows_League",
#         "reds_League",
#         "mins_played_League"
#       )
#     ),
#     reactable::colGroup(
#       name = "FA Cup",
#       columns = c(
#         "apps_FA Cup",
#         "starts_FA Cup",
#         "sub_apps_FA Cup",
#         "goals_FA Cup",
#         "yellows_FA Cup",
#         "reds_FA Cup",
#         "mins_played_FA Cup"
#       )
#     ),
#     reactable::colGroup(
#       name = "League Cup",
#       columns = c(
#         "apps_League Cup",
#         "starts_League Cup",
#         "sub_apps_League Cup",
#         "goals_League Cup",
#         "yellows_League Cup",
#         "reds_League Cup",
#         "mins_played_League Cup"
#       )
#     ),
#     reactable::colGroup(
#       name = "Other",
#       columns = c(
#         "apps_Other",
#         "starts_Other",
#         "sub_apps_Other",
#         "goals_Other",
#         "yellows_Other",
#         "reds_Other",
#         "mins_played_Other"
#       )
#     )
#   )
# )
