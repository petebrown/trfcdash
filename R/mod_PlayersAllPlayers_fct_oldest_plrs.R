get_oldest_players <- function(df, n_records) {

  df <- df %>%
    dplyr::filter(
      !is.na(player_dob)
    ) %>%
    dplyr::mutate(
      plr_game_age = game_date - player_dob
    ) %>%
    dplyr::select(
      menu_name,
      plr_game_age,
      game_date
    ) %>%
    dplyr::slice_max(
      order_by = plr_game_age,
      n = ifelse(n_records == "all", length(.$menu_name), n_records)
    ) %>%
    dplyr::mutate(
      max_age_yrs = floor(as.numeric(plr_game_age / 365.25)),
      max_age_days = ceiling(plr_game_age - (max_age_yrs * 365.25)),
      game_date = max(game_date)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      dplyr::desc(plr_game_age)
    ) %>%
    dplyr::mutate(
      rank = dplyr::min_rank(dplyr::desc(plr_game_age))
    ) %>%
    dplyr::select(
      rank,
      menu_name,
      plr_game_age,
      game_date,
      max_age_yrs,
      max_age_days
    )

  reactable::reactable(
    data = df,
    class = "apps-reactable",
    style = list(
      fontSize = "0.8rem",
      fontWeight = 300
    ),
    rowClass = "results-row",
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "plr_game_age",
    defaultColDef = reactable::colDef(
      vAlign = "top",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    columns = list(
      rank = reactable::colDef(
        name = "",
        width = 41
      ),
      menu_name = reactable::colDef(
        name = "Player",
        minWidth = 115
      ),
      plr_game_age = reactable::colDef(
        name = "Age",
        align = "right",
        cell = reactable::JS("function(cellInfo, state) {
          const age = cellInfo.value;
          const years = Math.floor(age / 365.25);
          const days = Math.ceil(age - (years * 365.25));

          return `${years} yrs, ${days} days`;
        }"
        ),
        html = TRUE
      ),
      game_date = reactable::colDef(
        name = "Game Date",
        align = "right",
        width = 95,
        format = reactable::colFormat(
          date = TRUE,
          locales = "en-GB"
        ),
      ),
      max_age_yrs = reactable::colDef(
        show = FALSE
      ),
      max_age_days = reactable::colDef(
        show = FALSE
      )
    )
  )

}
