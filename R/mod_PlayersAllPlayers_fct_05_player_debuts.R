get_player_debuts <- function(df=results_dataset) {
  game_dates <- df %>%
    dplyr::ungroup() %>%
    dplyr::pull(game_date) %>%
    unique()

  df <- player_debuts %>%
    dplyr::filter(
      debut_date %in% game_dates
    )

  player_headshots <- player_imgs %>%
    dplyr::select(menu_name=pl_index, plr_headshot=headshot_file_path)
  plr_positions <- player_positions %>%
    dplyr::select(menu_name=pl_index, position)

  df <- df %>%
    dplyr::left_join(
      player_headshots,
      by = "menu_name"
    ) %>%
    dplyr::left_join(
      plr_positions,
      by = "menu_name"
    ) %>%
    dplyr::mutate(
      Rank = dplyr::row_number()
    ) %>%
    dplyr::select(
      Rank,
      menu_name,
      plr_game_age,
      debut_date,
      age_yrs,
      age_days,
      plr_headshot,
      position
    )

  reactable::reactable(
    df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "asc",
    defaultSorted = "plr_game_age",
    defaultColDef = reactable::colDef(
      vAlign = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    compact = TRUE,
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(nrow(df)),
    columns = list(
      Rank = reactable::colDef(
        name = "",
        width = 30,
        align = "right",
        cell = reactable::JS("function(cellInfo, state) {
          return cellInfo.viewIndex + 1;
        }"),
      ),
      menu_name = reactable::colDef(
        name = "Player",
        defaultSortOrder = "asc",
        cell = plr_name_and_headshot(),
        html = TRUE
      ),
      debut_date = reactable::colDef(
        name = "Debut Date",
        minWidth = 100,
        align = "right",
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        )
      ),
      plr_game_age = reactable::colDef(
        name = "Age on Debut",
        minWidth = 100,
        sortNALast = TRUE,
        cell = reactable::JS("function(cellInfo, state) {
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
      player_dob = reactable::colDef(
        name = "Date of Birth",
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        )
      ),
      age_yrs = reactable::colDef(
        show = FALSE
      ),
      age_days = reactable::colDef(
        show = FALSE
      ),
      player_name = reactable::colDef(
        show = FALSE
      ),
      plr_headshot = reactable::colDef(
        show = FALSE
      ),
      position = reactable::colDef(
        show = FALSE
      )
    )
  )
}
