############################
# SOURCE CLUB CREST IMAGES #
############################

clubs_crests <- data.frame(
  club = sort(unique(results_dataset$opposition))
) %>%
  dplyr::mutate(
    img = stringr::str_to_lower(club) %>%
      stringr::str_replace_all(" ", "-") %>%
      stringr::str_replace_all("'", "") %>%
      stringr::str_replace_all("-u21", "") %>%
      stringr::str_replace_all("-academy", ""),
    file_path = dplyr::case_when(
      file.exists(paste0("./inst/app/www/images/clubs/", img, ".svg")) ~ paste0("./www/images/clubs/", img, ".svg"),
      file.exists(paste0("./inst/app/www/images/clubs/", img, ".png")) ~ paste0("./www/images/clubs/", img, ".png"),
      file.exists(paste0("./inst/app/www/images/clubs/", img, ".jpg")) ~ paste0("./www/images/clubs/", img, ".jpg"),
      TRUE ~ "./www/images/clubs/placeholder.svg"
    )
  )

na_crests <- clubs_crests %>%
  dplyr::filter(is.na(file_path)) %>%
  dplyr::pull(club)

usethis::use_data(
  clubs_crests,
  na_crests,
  overwrite = TRUE
)

############################
# SOURCE COMPETITION LOGOS #
############################

comp_logos <- data.frame(
  generic_comp = sort(unique(results_dataset$generic_comp))
) %>%
  dplyr::mutate(
    img = stringr::str_to_lower(generic_comp) %>%
      stringr::str_replace_all(" ", "-") %>%
      stringr::str_replace_all("'", ""),
    file_path = dplyr::case_when(
      file.exists(paste0("./inst/app/www/images/competitions/", img, ".svg")) ~ paste0("./www/images/competitions/", img, ".svg"),
      file.exists(paste0("./inst/app/www/images/competitions/", img, ".png")) ~ paste0("./www/images/competitions/", img, ".png"),
      file.exists(paste0("./inst/app/www/images/competitions/", img, ".jpg")) ~ paste0("./www/images/competitions/", img, ".jpg"),
      TRUE ~ "./www/images/clubs/placeholder.svg"
    )
  )

spec_comp_logos <- results_dataset %>%
  dplyr::select(
    season,
    generic_comp,
    competition
  ) %>%
  unique() %>%
  dplyr::left_join(
    comp_logos,
    by = "generic_comp"
  ) %>%
  dplyr::mutate(
    file_path_2 = dplyr::case_when(
      generic_comp == "Football League" & season < 1988 ~ "./www/images/competitions/football-league-1.png",
      generic_comp == "Football League" & season < 2004 ~ "./www/images/competitions/football-league-2.svg",
      generic_comp == "Football League" & season < 2016 ~ "./www/images/competitions/football-league-3.png",
      generic_comp == "Football League" & season > 2016 ~ "./www/images/competitions/football-league.svg",
      TRUE ~ file_path
    )
  )

usethis::use_data(
  comp_logos,
  spec_comp_logos,
  overwrite = TRUE
)
