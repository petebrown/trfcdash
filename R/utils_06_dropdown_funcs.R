#' 06_dropdown_funcs
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

results_with_subtable <- function(df, inc_cup_games="Yes", drop_cols=c(), show_details='No', show_imgs='No') {

  results_df = df %>%
    dplyr::select(
      season,
      game_no,
      game_date,
      venue,
      opposition,
      outcome,
      score,
      game_type,
      competition,
      attendance,
      manager,
      league_tier
    )

  if (inc_cup_games == "No") {
    results_df <- results_df %>%
      dplyr::filter(
        game_type == "League"
      ) %>%
      dplyr::arrange(
        season,
        game_no
      ) %>%
      dplyr::mutate(
        game_no = dplyr::row_number()
      )
  }

  players_df = player_apps %>%
    dplyr::filter(game_date %in% results_df$game_date) %>%
    dplyr::arrange(
      game_date,
      role,
      shirt_no
    ) %>%
    dplyr::select(
      season,
      game_date,
      shirt_no,
      menu_name,
      role,
      mins_played,
      goals_scored,
      yellow_cards,
      red_cards
    )

  lge_tabs_df = lge_tables %>%
    dplyr::filter(game_date %in% results_df$game_date) %>%
    dplyr::arrange(
      season,
      game_no,
      pos
    )

  crest_list <- as.list(clubs_crests %>%
    dplyr::select(club, file_path) %>%
    dplyr::pull(file_path) %>%
    purrr::set_names(clubs_crests %>% dplyr::pull(club)))


  comp_logos <- spec_comp_logos %>%
    dplyr::select(season, competition, comp_logo=file_path_2)

  results_df <- results_df %>%
    dplyr::left_join(
      comp_logos,
      by = c("season", "competition")
    )

  player_headshots <- player_imgs %>%
    dplyr::select(menu_name=pl_index, plr_headshot=headshot_file_path)
  plr_positions <- player_positions %>%
    dplyr::select(menu_name=pl_index, position)

  players_df <- players_df %>%
    dplyr::left_join(
      player_headshots,
      by = "menu_name"
    ) %>%
    dplyr::left_join(
      plr_positions,
      by = "menu_name"
    )


  reactable::reactable(
    data = results_df,
    meta = list(
      crests = crest_list
    ),
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    showPageSizeOptions = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = get_page_nos(length(results_df$season)),
    compact = TRUE,
    searchable = TRUE,
    borderless = TRUE,
    filterable = FALSE,
    columns = list(
      season = reactable::colDef(
        name = "Season",
        minWidth = 75
      ),
      game_no = reactable::colDef(
        name = "Game",
        align = "center",
        minWidth = 70
      ),
      game_date = reactable::colDef(
        name = "Date",
        minWidth = 105,
        format = reactable::colFormat(date = TRUE, locales = "en-GB")
      ),
      venue = reactable::colDef(
        name = "Venue",
        minWidth = 70,
        align = "center"
      ),
      opposition = reactable::colDef(
        name = "Opponent",
        minWidth = 230,
        cell = reactable::JS("function(cellInfo, state) {
          const { crests } = state.meta;

          let opponent = cellInfo.value;
          let opponent_text = opponent;
          let img_src = crests[opponent];

          let venue = cellInfo.row['venue'];

          if (venue == 'H') {
            opponent_text = opponent_text.toUpperCase();
          };

          img = `<img src='${img_src}' style='height:32px; margin:2px;' alt='${opponent}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:40px;'>${img}</div>
            <div style='display:flex; text-align:left; margin:6.4px;'>${opponent_text}</div>
          </div>
          `
        }"),
        html = TRUE,
        style = function(value, index) {
          if (results_df$venue[index] == "H") {
            font_weight = "450"
          } else {
            font_weight = "300"
          }
          list(
            fontWeight = font_weight
          )
        }
      ),
      outcome = reactable::colDef(
        name = "Res",
        align = "center",
        minWidth = 40,
        show = ifelse('outcome' %in% drop_cols, FALSE, TRUE)
      ),
      score = reactable::colDef(
        name = "Score",
        minWidth = 70,
        align = "center"
      ),
      game_type = reactable::colDef(
        show = FALSE
      ),
      competition = reactable::colDef(
        name = "Competition",
        minWidth = 200,
        vAlign = "center",
        cell = reactable::JS("function(cellInfo) {
          let img_src = cellInfo.row['comp_logo'];
          let competition = cellInfo.value;

          img = `<img src='${img_src}' style='height:32px; margin:2px;' alt='${competition}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:40px;'>${img}</div>
            <div style='display:flex; text-align:left; margin:10px;'>${competition}</div>
          </div>
          `
        }"),
        html = TRUE
      ),
      attendance = reactable::colDef(
        name = "Att.",
        minWidth = 70,
        format = reactable::colFormat(
          digits = 0,
          separators = TRUE
        )
      ),
      manager = reactable::colDef(
        name = "Manager",
        minWidth = 120,
        align = "right"
      ),
      league_tier = reactable::colDef(show = FALSE),
      comp_logo = reactable::colDef(show = FALSE)
    ),
    rowClass = "results-row",
    details = if (show_details == 'Yes') {function(index) {
      line_up = players_df[players_df$game_date == results_df$game_date[index], ]
      lge_tab = lge_tabs_df[lge_tabs_df$game_date == results_df$game_date[index], ]
      div(
        style='display:flex; flex-wrap:wrap; gap:0 100px; max-width:1500px;',
        div(
          class = "reactable-details",
          style = "flex-grow:45;",
          bslib::card_title("Line-up"),
          reactable::reactable(
            data = line_up,
            class = "reactable-text",
            defaultColDef = reactable::colDef(
              vAlign = "top",
              headerClass = "bar-sort-header"
            ),
            showSortIcon = FALSE,
            outlined = FALSE,
            bordered = FALSE,
            borderless = TRUE,
            defaultPageSize = 16,
            compact = TRUE,
            filterable = FALSE,
            resizable = TRUE,
            columns = list(
              season = reactable::colDef(show = FALSE),
              game_date = reactable::colDef(show = FALSE),
              shirt_no = reactable::colDef(
                name = "No.",
                align = "center",
                minWidth = 35
              ),
              menu_name = reactable::colDef(
                name = "Player",
                minWidth = 200,
                cell = reactable::JS("function(cellInfo, state) {
                  let player = cellInfo.value;
                  let plr_name = player.split(' (b.')[0];
                  let plr_pos = cellInfo.row['position'];
                  let img_src = cellInfo.row['plr_headshot'];

                  img = `<img src='${img_src}' style='height:30px; margin:auto;' alt='${plr_name}'>`;

                  player = player.split(' (b.')[0];

                  return `
                  <div style='display: flex'>
                    <div style='display:flex; justify-content:center; width:35px;'>${img}</div>
                    <div style='text-align:left; margin:auto 10px; line-height: 1rem;'>
                      ${plr_name}
                      <br>
                      <span style='font-size: smaller; color: #aaa9a9; line-height: 1.1rem;'>${plr_pos}</span>
                    </div>
                  </div>
                  `
                }"),
                html = TRUE
              ),
              role = reactable::colDef(
                name="Role",
                align = "left",
                minWidth = 70,
                cell = function(value) {
                  stringr::str_to_title(value)
                }
              ),
              mins_played = reactable::colDef(
                name = "⏱️",
                align="center",
                minWidth = 40,
                defaultSortOrder = "desc"
              ),
              goals_scored = reactable::colDef(
                name = "⚽️",
                align="center",
                minWidth = 40,
                defaultSortOrder = "desc"
              ),
              yellow_cards = reactable::colDef(
                name = "🟨",
                align="center",
                minWidth = 40,
                defaultSortOrder = "desc"
              ),
              red_cards = reactable::colDef(
                name = "🟥",
                align="center",
                minWidth = 40,
                defaultSortOrder = "desc"
              ),
              plr_headshot = reactable::colDef(show = FALSE),
              position = reactable::colDef(show = FALSE)
            ),
            rowStyle = function(index) {
              if (line_up[index, "role"] == "sub") {
                list(
                  background = "rgba(0, 0, 0, 0.03)"
                )
              }
            }
          )
        ),
        if (!is.na(results_df[index, "league_tier"])) {
          div(
            class = "reactable-details",
            style = "flex-grow: 55;",
            bslib::card_title("As It Stood"),
            reactable::reactable(
              data = lge_tab,
              meta = list(
                crests = crest_list
              ),
              class = "reactable-text lge_tab",
              showSortIcon = FALSE,
              defaultColDef = reactable::colDef(
                vAlign = "center",
                headerClass = "bar-sort-header",
                defaultSortOrder = "desc"
              ),
              defaultPageSize = 24,
              compact = TRUE,
              bordered = FALSE,
              borderless = TRUE,
              outlined = FALSE,
              filterable = FALSE,
              searchable = FALSE,
              resizable  = TRUE,
              columns    = list(
                season = reactable::colDef(show = FALSE),
                game_no = reactable::colDef(show = FALSE),
                game_date = reactable::colDef(show = FALSE),
                pos = reactable::colDef(
                  name = "Pos",
                  align = "left",
                  width = 37,
                  defaultSortOrder = "asc"
                ),
                Team = reactable::colDef(
                  minWidth = 200,
                  defaultSortOrder = "asc",
                  cell = reactable::JS("function(cellInfo, state) {
                    const { crests } = state.meta;
                    const team = cellInfo.value;
                    let opponent = cellInfo.value;
                    let img_src = crests[opponent];

                    if (opponent === 'Tranmere Rovers') {
                      img_src = './www/images/clubs/tranmere-rovers.svg';
                    }

                    img = `<img src='${img_src}' style='height:25px; margin:2px;' alt='${opponent}'>`;

                    return `
                    <div style='display: flex'>
                      <div style='display:flex; justify-content:center; width:40px;'>${img}</div>
                      <div style='display:flex; text-align:left; margin:5px;'>${opponent}</div>
                    </div>
                    `
                  }"),
                  html = TRUE
                ),
                Pld = reactable::colDef(width = 40),
                W = reactable::colDef(width = 40),
                D = reactable::colDef(width = 40),
                L = reactable::colDef(width = 40),
                GF = reactable::colDef(width = 40),
                GA = reactable::colDef(width = 40),
                GD = reactable::colDef(
                  width = 50,
                  # Function to add plus sign (+) before positive figures
                  cell = function(value) {
                    format_gd(value)
                  }
                ),
                Pts = reactable::colDef(width = 40)
              ),
              rowStyle = function(index) {
                tier = results_df[results_df$game_date == lge_tab$game_date[index], ]$league_tier
                season = lge_tab[index, "season"]
                pos = lge_tab[index, "pos"]
                team = lge_tab[index, "Team"]

                styles = list()
                if (team == "Tranmere Rovers") {
                  styles = c(styles, fontWeight = "bold")
                }

                if (
                  tier %in% c(2, 3, 5) & pos >= 21 |
                  tier == 4 & pos >= 23
                ) {
                  styles = c(styles, background = "rgba(0, 0, 0, 0.03)")
                }

                if (
                  tier %in% c(2, 3) & (pos %in% c(2, 6)) |
                  tier == 4 & (pos %in% c(3, 7)) |
                  tier == 5 & (pos %in% c(1, 3, 7))
                ) {
                  styles = c(styles, borderBottom = "1px solid rgba(0, 0, 0, 0.1)")
                }
                return(styles)
              }
            )
          )
        } else {
          div()
        }
      )
    }}
  )
}
