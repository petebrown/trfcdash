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
      Rank = dplyr::row_number(),
      rank_by_name = dplyr::min_rank(menu_name),
      rank_by_game_age = dplyr::min_rank(plr_game_age),
      rank_by_debut_date = dplyr::min_rank(debut_date)
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

  debut_ages <- df %>%
    dplyr::filter(!is.na(plr_game_age)) %>%
    dplyr::pull(plr_game_age)

  debut_dates <- df %>%
    dplyr::filter(!is.na(debut_date)) %>%
    dplyr::pull(debut_date)



  reactable::reactable(
    df,
    meta = list(
      debut_ages = debut_ages,
      debut_dates = debut_dates
    ),
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
        width = 40,
        align = "right",
        cell = reactable::JS("function(cellInfo, state) {
          let debut_ages = state.meta.debut_ages;
          let debut_dates = state.meta.debut_dates;

          let sort_col = state.sorted[0]['id'];
          console.log(sort_col);

          if (sort_col === 'plr_game_age') {
            console.log('sorting by game age');
          } else if (sort_col === 'debut_date') {
            console.log('sorting by debut date');
          }

          let data = [];

          if (sort_col === 'plr_game_age') {
            data = state.meta.debut_ages;
          } else if (sort_col === 'debut_date') {
            data = state.meta.debut_dates;
          }
          console.log(data.length)

          let all_values = [];
          for (let i = 0; i < data.length; i++) {
            val = data[i];
            if (val !== null) {
              all_values.push(val);
            }
          }

          const sortedValues = [...all_values].sort((a, b) => a - b);

          const rankMap = new Map();
          let rank = 1;

          sortedValues.forEach((value, index) => {
            if (!rankMap.has(value)) {
              rankMap.set(value, rank);
              rank++;
            }
          });

          const rankedValues = all_values.map(value => rankMap.get(value));

          function getRank(value) {
            return rankMap.get(value);
          }

          const valueToFind = cellInfo.row[sort_col];
          const rankOfValue = getRank(valueToFind);

          return rankOfValue;
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
