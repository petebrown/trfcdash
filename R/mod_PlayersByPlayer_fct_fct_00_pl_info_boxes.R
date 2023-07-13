get_pl_debut <- function(pl_name) {
  player_apps %>%
    dplyr::filter(
      player_name == pl_name
    ) %>%
    dplyr::select(
      -season
    ) %>%
    dplyr::left_join(
      results_dataset,
      by = dplyr::join_by(game_date)
    ) %>%
    dplyr::slice_min(
      game_date,
      n = 1
    ) %>%
    dplyr::mutate(
      game_date = stringr::str_glue("{lubridate::day(game_date)} {lubridate::month(game_date, label = TRUE, abbr = TRUE)} {lubridate::year(game_date)}")
    )
}

pl_debut_date <- function(pl_name) {
  get_pl_debut(pl_name)["game_date"]
}
pl_debut_competition <- function(pl_name) {
  comp <- get_pl_debut(pl_name)["competition"]
  ssn <- get_pl_debut(pl_name)["season"]

  stringr::str_glue("{comp}, {ssn}")
}
pl_debut_match <- function(pl_name) {
  pl_debut_score <- get_pl_debut(pl_name)["score"]
  pl_debut_opposition <- get_pl_debut(pl_name)["opposition"]
  pl_debut_venue <- get_pl_debut(pl_name)["venue"]

  stringr::str_glue("{pl_debut_score} v {pl_debut_opposition} ({pl_debut_venue})")
}

get_pl_summary <- function(pl_name) {
  player_apps %>%
    dplyr::filter(
      player_name == pl_name
    ) %>%
    dplyr::left_join(
      results_dataset %>% dplyr::select(game_date, game_type),
      by = "game_date"
    ) %>%
    tidyr::replace_na(
      list(
        goals_scored = 0
      )
    ) %>%
    dplyr::group_by(
      season
    ) %>%
    dplyr::summarise(
      total_apps = dplyr::n(),
      starts = sum(role == "starter"),
      sub_apps = sum(role == "sub"),
      goals = sum(goals_scored),
      lge_gls = sum(goals_scored[game_type == "League"]),
      cup_gls = sum(goals_scored[game_type != "League"])
    )
}

pl_apps <- function(pl_name) {
  starts <- (sum(get_pl_summary(pl_name)$starts))
  sub_apps <- (sum(get_pl_summary(pl_name)$sub_apps))

  stringr::str_glue("{starts} ({sub_apps})")
}
pl_total_gls <- function(pl_name) {
  sum(get_pl_summary(pl_name)$goals)
}
pl_max_gls_ssn <- function(pl_name) {
  get_pl_summary(pl_name) %>%
    dplyr::slice_max(
      goals,
      n = 1
    )
}
pl_gls_totals <- function(pl_name) {
  lge_gls = sum(get_pl_summary(pl_name)$lge_gls)
  cup_gls = sum(get_pl_summary(pl_name)$cup_gls)

  lg_out = stringr::str_glue("{lge_gls} league")
  cup_out = stringr::str_glue("{cup_gls} cup")

  list(lg_out, cup_out)
}

pl_best_ssn_desc <- function(pl_name) {
  icon = bsicons::bs_icon("1-circle")
  gls = pl_max_gls_ssn(pl_name)$goals
  ssn = pl_max_gls_ssn(pl_name)$season

  stringr::str_glue("{icon} {gls}, {ssn}")
}

pl_min_ssn <- function(pl_name) {
  min(get_pl_summary(pl_name)$season)
}
pl_max_ssn <- function(pl_name) {
  max(get_pl_summary(pl_name)$season)
}
pl_ssns <- function(pl_name) {
  min_ssn <- min(get_pl_summary(pl_name)$season)
  max_ssn <- max(get_pl_summary(pl_name)$season)

  if (max_ssn == min_ssn) {
    min_ssn
  } else {
    stringr::str_glue("{min_ssn} - {max_ssn}")
  }
}

pl_win_pc <- function(pl_name) {
  df <- player_apps %>%
    dplyr::filter(
      player_name == pl_name,
    ) %>%
    dplyr::left_join(
      results_dataset %>% dplyr::select(game_date, game_type, outcome),
      by = "game_date"
    ) %>%
    dplyr::summarise(
      apps = dplyr::n(),
      wins = sum(outcome == "W"),
      win_pc = round((wins / apps) * 100, 1),
      win_pc = stringr::str_glue("{win_pc}%")
    )
}

pl_win_pc_desc <- function(pl_name) {
  wins = pl_win_pc(pl_name)$wins
  app = pl_win_pc(pl_name)$apps
  plural = ifelse(wins == 1, "win", "wins")

  stringr::str_glue("{wins} {plural} in {app} apps")
}


pl_value_boxes <- function(player_name) {
  bslib::card(
    class = c("borderless", "no_padding"),
    bslib::card_body(
      class = "no_padding",
      bslib::layout_column_wrap(
        width = 1/5,

        bslib::value_box(
          title = "Debut",
          value = as.character(pl_debut_date(player_name)),
          # showcase = bsicons::bs_icon("calendar-check"),
          p(pl_debut_match(player_name)),
          p(pl_debut_competition(player_name))
        ),
        bslib::value_box(
          title = "Appearances",
          value = as.character(pl_apps(player_name)),
          # showcase = bsicons::bs_icon("list-ol"),
          p(pl_ssns(player_name))
        ),
        bslib::value_box(
          title = "Goals",
          value = pl_total_gls(player_name),
          # showcase = bsicons::bs_icon("bar-chart"),
          p(pl_gls_totals(player_name)[1]),
          p(pl_gls_totals(player_name)[2])
        ),
        bslib::value_box(
          title = "Most goals in a season",
          value = ifelse(
            pl_total_gls(player_name) == 0,
            "0",
            pl_max_gls_ssn(player_name)$goals
          ),
          # showcase = bsicons::bs_icon("bar-chart"),
          p(ifelse(
            pl_total_gls(player_name) == 0,
            "",
            pl_max_gls_ssn(player_name)$season
            )
          )
        ),
        bslib::value_box(
          title = "Win rate",
          value = as.character(pl_win_pc(player_name)$win_pc),
          # showcase = fontawesome::fa("r-project", fill = "steelblue"),
          p(pl_win_pc_desc(player_name))
        )
      )
    )
  )
}
