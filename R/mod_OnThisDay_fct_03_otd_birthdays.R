get_otd_birthdays <- function(otd_date, inc_year = "No") {

  df <- player_positions %>%
    dplyr::group_by(
      pl_index
    ) %>%
    dplyr::slice_head(
      n = 1
    ) %>%
    dplyr::filter(
      lubridate::month(player_dob) == lubridate::month(otd_date),
      lubridate::day(player_dob) == lubridate::day(otd_date),
      dplyr::case_when(
        inc_year == "Yes" ~ lubridate::year(player_dob) == lubridate::year(otd_date),
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::arrange(
      dplyr::desc(player_dob)
    ) %>%
    dplyr::select(
      pl_index,
      player_dob
    )


  reactable::reactable(
    data = df,
    meta = list(
      plr_headshots = meta_data[['plr_headshots']],
      plr_positions = meta_data[['plr_positions']]
    ),
    columns = list(
      pl_index = reactable::colDef(
        name = "Player",
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
      player_dob = reactable::colDef(
        name = "Date of Birth",
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        )
      )
    )
  )

}
