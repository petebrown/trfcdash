#' 03_table_utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


font_style <- "color: black; font-weight: 200; font-size: smaller;"

# Function to add plus sign (+) before positive figures
format_gd <- function(value) {
  if (value != 0)
    sprintf("%+3d", value)
  else
    value
}


get_mgr_role <- function(df) {
  managers <- managers %>%
    dplyr::rename(
      mgr_role = role
    )

  df %>%
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
    )
}


get_page_nos <- function(n) {
  i <- 10
  numbers <- c()
  while (n >= i) {
    # add n to list
    numbers <- c(numbers, i)
    # increass n by 10
    i <- i + 10
  }
  if (n %% 10 != 0) {
    numbers <- c(numbers, n)
  }
  return(numbers)
}


generate_streaks <- function(df, drop_games_played = TRUE) {
  streaks <- df %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      blanks = ifelse(goals_for == 0, 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      wins_cs = ifelse(outcome == "W" & goals_against == 0, 1, 0),
      defeats_to_0 = ifelse(outcome == "L" & goals_for == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      blanks_streak = ifelse(blanks == 0, 0, sequence(rle(as.character(blanks))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
      wins_to_nil = ifelse(wins_cs == 0, 0, sequence(rle(as.character(wins_cs))$lengths)),
      defeats_to_nil = ifelse(defeats_to_0 == 0, 0, sequence(rle(as.character(defeats_to_0))$lengths))
    ) %>%
    dplyr::summarize(
      P = dplyr::n(),
      wins = max(w_streak),
      unbeaten = max(unbeaten_streak),
      clean_sheets = max(clean_sheets),
      wins_to_nil = max(wins_to_nil),
      draws = max(d_streak),
      defeats = max(losing_streak),
      winless = max(winless_streak),
      Blanks = max(blanks_streak),
      defeats_to_nil = max(defeats_to_nil),
      .groups = "drop"
    )

  # Remove P column if drop_games_played is TRUE
  if (drop_games_played) {
    streaks <- streaks %>%
      dplyr::select(-P)
  }

  return(streaks)

}

format_streak_cols <- function() {
  list(
    wins = reactable::colDef(
      name = "Wins"
    ),
    unbeaten = reactable::colDef(
      name = "Unbeaten"
    ),
    clean_sheets = reactable::colDef(
      name = "Clean Sheets"
    ),
    wins_to_nil = reactable::colDef(
      name = "Wins to nil"
    ),
    draws = reactable::colDef(
      name = "Draws"
    ),
    defeats = reactable::colDef(
      name = "Defeats"
    ),
    winless = reactable::colDef(
      name = "Winless"
    ),
    defeats_to_nil = reactable::colDef(
      name = "Defeats to nil"
    )
  )
}

streaks_reactable <- function(df) {
  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    columns = format_streak_cols()
  )
}

summarise_results <- function(df) {
  df %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = GF - GA,
      win_pc = W / P,
      .groups = "drop"
    )
}

generate_record <- function(df) {
  df %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against),
      GD = sum(goals_for) - sum(goals_against),
      win_pc = W / P,
      Pts = dplyr::case_when(
        sum(game_type == "League") > 0 ~ (sum(game_type == "League" & outcome == "W") * 3) + sum(game_type == "League" & outcome == "D"),
        TRUE ~ NA
      ),
      PPG = Pts / sum(game_type == "League"),
      .groups = "drop"
    )
}

mutate_pens_as_draw <- function(df, pens_as_draw) {
  df %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        pens_as_draw == "No" & decider == "pens" & is.na(cup_leg) ~ cup_outcome,
        .default = outcome
      )
    )
}

filter_inc_cup_games <- function(df, inc_cup_games) {
  df %>%
    dplyr::filter(
      dplyr::case_when(
        inc_cup_games == "No" ~ game_type == "League",
        TRUE ~ TRUE
      )
    )
}


# Javascript function to return the total of a column in a Reactable table
js_total_col <- function() {
  reactable::JS("function(colInfo) {
        var total = 0
        colInfo.data.forEach(function(row) {
          total += row[colInfo.column.id]
        })
        return total
      }"
    )
}

make_color_pal <- function(colors, bias = 1) {
  get_color <- grDevices::colorRamp(colors, bias = bias)
  function(x) grDevices::rgb(get_color(x), maxColorValue = 255)
}

green_pal <- make_color_pal(c("grey85", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 0.7)


with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy::tippy(value, tooltip, ...))
}

matchday_tooltip <- function(game_df) {
  game_no = game_df %>% dplyr::pull(game_no)
  date = game_df %>% dplyr::pull(game_date)
  opposition = game_df %>% dplyr::pull(opposition)
  venue = game_df %>% dplyr::pull(venue)
  outcome_desc = game_df %>% dplyr::pull(outcome_desc)
  competition = game_df %>% dplyr::pull(competition)
  score = game_df %>% dplyr::pull(score)
  goals_for = game_df %>% dplyr::pull(goals_for)
  scorers = game_df %>% dplyr::pull(scorers)
  attendance = game_df %>% dplyr::pull(attendance)
  manager = game_df %>% dplyr::pull(manager)

  theme_col = ifelse(venue == "H", "light", "material")

  if (!is.na(scorers)) {
    scorers = paste0("<i>", scorers, "</i><br>")
  }

  tooltip = paste0(
    "<b>", format(lubridate::ymd(date), "%e %b %Y"), "</b><br>",
    competition, "<br>",
    opposition, " (", venue, ")<br>",
    score, ifelse(!is.na(outcome_desc), paste0(" (", outcome_desc, ")"), ""), "<br>",
    if (!is.na(scorers)) {
      scorers
    },
    if (!is.na(attendance)) {
      paste0(format(attendance, big.mark = ","), "<br>")
    },
    manager
  )

  div(
    style = "text-decoration: underline; text-decoration-style: dotted; cursor: help;",
      tippy::tippy(
        game_no,
        tooltip,
        theme = theme_col,
        duration = c(300, 250)
      )
    )
}


opts_select_seasons <- function(ns, name) {
  selectInput(
    inputId = ns(paste(name, "selected_seasons", sep="_")),
    label = h6("Select seasons:"),
    choices = get_season_list(),
    selected = max(get_season_list()),
    multiple = TRUE
  )
}

opts_filter_years <- function(ns, name){
  sliderInput(
    inputId = ns(paste(name, "year_range", sep="_")),
    label = h6("Select season range:"),
    min = min(results_dataset$ssn_year),
    max = max(results_dataset$ssn_year),
    sep = "",
    ticks = FALSE,
    step = 1,
    value = c(
      min(results_dataset$ssn_year),
      max(results_dataset$ssn_year)
    )
  )
}

opts_filter_comps <- function(ns, name) {
  checkboxGroupInput(
    inputId = ns(paste(name, "cup_comps", sep="_")),
    label = h6("Cup competitions:"),
    choices = list(
      "Anglo-Italian Cup" = "Anglo-Italian Cup",
      "Associate Members' Cup" = "Associate Members\' Cup",
      "FA Cup" = "FA Cup",
      "FA Trophy" = "FA Trophy",
      "Full Members' Cup" = "Full Members\' Cup",
      "League Cup" = "League Cup",
      "War League" = "War League"
    ),
    selected = c(
      "Anglo-Italian Cup",
      "Associate Members\' Cup",
      "FA Cup",
      "FA Trophy",
      "Full Members\' Cup",
      "League Cup",
      "War League"
    )
  )
}

opts_inc_cup_games <- function(ns, name) {
  radioButtons(
    inputId = ns(paste(name, "inc_cup_games", sep = "_")),
    label = tags$b("Include cup games?"),
    choices = c("Yes", "No"),
    selected = "Yes",
    inline = TRUE
  )
}

popover_options <- function(ns) {
  bslib::popover(
    title = "Display Options",
    bsicons::bs_icon("gear"),
    radioButtons(
      inputId = ns("selected_stat"),
      label = tags$b("Matchday Stat:"),
      choiceNames = c(
        "Mins played",
        "Goals",
        "Cards"
      ),
      choiceValues = c(
        "mins_played",
        "goals_scored",
        "cards"
      ),
      inline = TRUE
    ),
    hr(),
    opts_inc_cup_games(ns, "app_react"),
    hr(),
    radioButtons(
      inputId = ns("app_react_pens_as_draw"),
      label = tags$b("Treat one-off cup games decided by penalty shoot-out as draws?"),
      choices = c("Yes", "No"),
      selected = "Yes",
      inline = TRUE
    ),
    hr(),
    sliderInput(
      inputId = ns("app_react_min_starts"),
      label = tags$b("Minimum no. of starts:"),
      min = 0,
      max = 50,
      value = 0,
      sep = "",
      ticks = FALSE,
      step = 1
    ),
    hr(),
    checkboxGroupInput(
      inputId = ns("player_roles"),
      label = tags$b("Show: "),
      choiceNames = c(
        "Starters",
        "Subs"
      ),
      choiceValues = c(
        "starter",
        "sub"
      ),
      selected = c(
        "starter",
        "sub"
      ),
      inline = TRUE
    ),
  )
}

club_and_crest <- function(value, venue="A") {
  image <- img(
    src = map_club_to_crest(value),
    style = "height: 32px; margin: 2px;",
    alt = value
  )

  if (venue == "H") {
    text = toupper(value)
  } else {
    text = value
  }

  tagList(
    div(style="display: flex",
        div(style = "display:flex; justify-content: center; width:40px;", image),
        div(style = "display: flex; text-align: left; margin: 10px;", text)
    )
  )
}

generic_comp_logo <- function(competition) {
  image <- img(
    src = map_competition_to_generic_logo(competition),
    style = "height: 32px; margin: 2px;",
    alt = competition
  )

  tagList(
    div(style="display: flex",
        div(style = "display:flex; justify-content: center; width:40px;", image),
        div(style = "display: flex; text-align: left; margin: 10px;", competition)
    )
  )
}
