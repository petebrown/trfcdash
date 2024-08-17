plr_shirt_nos <- function(player) {
  df <- player_apps %>%
    dplyr::filter(
      menu_name == player
    ) %>%
    dplyr::group_by(
      shirt_no
    ) %>%
    dplyr::summarize(
      n = dplyr::n()
    )

  reactable::reactable(
    data = df,
    searchable = TRUE,
    defaultSortOrder = "desc",
    defaultSorted = "n",
    showPageSizeOptions = TRUE,
    pageSizeOptions = get_page_nos(length(df$shirt_no)),
    defaultColDef = reactable::colDef(
      vAlign = "center",
      align = "center",
      headerClass = "bar-sort-header"
    ),
    showSortIcon = FALSE,
    class = "apps-reactable",
    style = list(
      fontSize = "0.9rem",
      fontWeight = 300
    ),
    columns = list(
      shirt_no = reactable::colDef(
        name = "Shirt No.",
        align = "center"
      ),
      n = reactable::colDef(
        name = "Appearances",
        align = "center"
      )
    )
  )
}


