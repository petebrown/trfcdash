get_player_teammates <- function(player) {
  game_dates <- player_apps %>%
    dplyr::filter(
      menu_name == player,
      role == 'starter'
    ) %>%
    dplyr::pull(game_date)

  df <- player_apps %>%
    dplyr::left_join(
      results_dataset %>%
        dplyr::select(
          -season,
          -game_no
        ),
      by = c('game_date')
    ) %>%
    dplyr::filter(
      game_date %in% game_dates,
      menu_name != player,
      role == 'starter'
    ) %>%
    dplyr::group_by(menu_name) %>%
    dplyr::summarize(
      P = dplyr::n(),
      W = sum(outcome == 'W'),
      D = sum(outcome == 'D'),
      L = sum(outcome == 'L'),
      GF = sum(goals_for),
      GA = sum(goals_against),
      clean_sheets = sum(goals_against == 0)
    ) %>%
    dplyr::arrange(
      dplyr::desc(P),
      menu_name
    )

  reactable::reactable(
    df,
    meta = list(
      positions = meta_plr_positions,
      headshots = meta_plr_headshots
    ),
    class = "apps-reactable",
    style = list(
      fontSize = "0.8rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      align = "center",
      headerClass = "bar-sort-header"
    ),
    defaultSortOrder = "desc",
    searchable = TRUE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(nrow(df)),
    columns = list(
      menu_name = reactable::colDef(
        name = "Teammate",
        show = TRUE,
        align = "left",
        minWidth = 200,
        defaultSortOrder = "asc",
        cell = reactable::JS("function(cellInfo, state) {
          const player = cellInfo.value;
          const plr_name = player.split(' (b.')[0];
          const plr_pos = state.meta.positions[player];
          const img_src = state.meta.headshots[player];

          img = `<img src='${img_src}' style='height:45px; margin:auto;' alt='${plr_name}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:50px;'>
              ${img}
            </div>
            <div style='text-align:left; margin:auto 10px; line-height:1rem;'>
              ${plr_name}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${plr_pos}
              </span>
            </div>
          </div>`
        }"),
        html = TRUE
      ),
      P = reactable::colDef(
        name = "P",
        style = list(
          fontSize = "large",
          fontWeight = 500
        )
      ),
      W = reactable::colDef(
        name = "W",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let games_played = cellInfo.row.P;

          let pc = (value / games_played) * 100;
          pc = pc.toFixed(1);

          return `
            <div style='line-height:1rem;'>
              ${value}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${pc}%
              </span>
            </div>`;
        }"),
        html = TRUE
      ),
      D = reactable::colDef(
        name = "D",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let games_played = cellInfo.row.P;

          let pc = (value / games_played) * 100;
          pc = pc.toFixed(1);

          return `
            <div style='line-height:1rem;'>
              ${value}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${pc}%
              </span>
            </div>`;
        }"),
        html = TRUE
      ),
      L = reactable::colDef(
        name = "L",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let games_played = cellInfo.row.P;

          let pc = (value / games_played) * 100;
          pc = pc.toFixed(1);

          return `
            <div style='line-height:1rem;'>
              ${value}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${pc}%
              </span>
            </div>`;
        }"),
        html = TRUE
      ),
      GF = reactable::colDef(
        name = "GF",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let games_played = cellInfo.row.P;

          let av = (value / games_played);
          av = av.toFixed(1);

          return `
            <div style='line-height:1rem;'>
              ${value}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${av}
              </span>
            </div>`;
        }"),
        html = TRUE
      ),
      GA = reactable::colDef(
        name = "GA",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let games_played = cellInfo.row.P;

          let av = (value / games_played);
          av = av.toFixed(1);

          return `
            <div style='line-height:1rem;'>
              ${value}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${av}
              </span>
            </div>`;
        }"),
        html = TRUE
      ),
      clean_sheets = reactable::colDef(
        name = "Clean Sheets",
        cell = reactable::JS("function(cellInfo) {
          let value = cellInfo.value;
          let games_played = cellInfo.row.P;

          let pc = (value / games_played) * 100;
          pc = pc.toFixed(1);

          return `
            <div style='line-height:1rem;'>
              ${value}
              <br>
              <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>
                ${pc}%
              </span>
            </div>`;
        }"),
        html = TRUE
      )
    )
  )


}
