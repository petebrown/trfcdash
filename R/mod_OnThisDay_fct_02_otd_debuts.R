get_otd_debuts <- function(otd_date, inc_year = "No") {

  df <- player_debuts %>%
    dplyr::filter(
      lubridate::month(debut_date) == lubridate::month(otd_date),
      lubridate::day(debut_date) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(debut_date) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::select(
      menu_name,
      plr_game_age,
      age_yrs,
      age_days
    )


  reactable::reactable(
    data = df,
    meta = list(
      plr_headshots = meta_data[['plr_headshots']],
      plr_positions = meta_data[['plr_positions']]
    ),
    columns = list(
      menu_name = reactable::colDef(
        name = "Player",
        minWidth = 175,
        defaultSortOrder = "asc",
        cell = reactable::JS("function(cellInfo, state) {
          const player = cellInfo.value;
          const plr_name = player.split(' (b.')[0];
          const plr_pos = state.meta.plr_positions[player];
          const img_src = state.meta.plr_headshots[player];

          img = `<img src='${img_src}' style='height:45px; margin:auto;' alt='${plr_name}'>`;

          return `
          <div style='display: flex'>
            <div style='display:flex; justify-content:center; width:50px;'>${img}</div>
              <div style='text-align:left; margin:auto 10px; line-height:1rem;'>
                ${plr_name}
                <br>
                <span style='font-size: smaller; color:#aaa9a9; line-height:1.1rem;'>${plr_pos}</span>
              </div>
            </div>
            `
        }"),
        html = TRUE
      ),
      plr_game_age = reactable::colDef(
        name = "Age on Debut",
        sortNALast = TRUE,
        cell = reactable::JS("function(cellInfo) {
          let age = cellInfo.value;
          let age_yrs = cellInfo.row.age_yrs;
          let age_days = cellInfo.row.age_days;
          let age_display = '';

          if (typeof age === 'number') {
            age_display = age_yrs + ' years, ' + age_days + ' days';
            age = age.toLocaleString() + ' days';
          } else {
            age_display = 'Unknown';
            age = '';
          }

          return `
            <div style='display:flex; flex-direction:column;'>
              <span style='line-height:1rem;'>${age_display}</span>
              <span style='font-size:smaller; color:#aaa9a9; line-height:1.1rem;'>${age}</span>
            </div>
          `;
        }"),
        html = TRUE
      ),
      age_yrs = reactable::colDef(
        show = FALSE
      ),
      age_days = reactable::colDef(
        show = FALSE
      )
    )
  )
}
