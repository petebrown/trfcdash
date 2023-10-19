#' 02_chart_utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "1.2rem", fill = "#00bfc4", background = NULL) {
  bar <- div(
    style = list(
      background = fill,
      width = width,
      height = height)
  )

  chart <- div(
    style = list(
      flexGrow = 1,
      marginLeft = "10%",
      marginRight = "1.0rem",
      background = background,
      `border-style` = "solid",
      `border-color` = "slategrey",
      `border-width` = "thin"
    ),
    bar
  )

  # Output div containing chart and label
  div(
    style = list(
      display = "flex",
      alignItems = "center"
    ),
    chart,
    label
  )
}

# Render a Reactable table with an image
mgr_img_table <- function(df, col_sort="P") {
  reactable::reactable(
    data = df,
    defaultSortOrder = "desc",
    defaultSorted = c(
      col_sort
    ),
    columns = list(
      manager = reactable::colDef(
        name = "",
        width = 75,
        vAlign = "top",
        cell = function(value) {
          image <- img(
            src = dplyr::case_when(
              .default = paste0(
                "./www/images/managers/", tolower(gsub(' ', '-', value)), ".jpg"),
              value == "No manager" ~ "./www/images/crest.svg",
              stringr::str_detect(value, "Sheedy") ~ "./www/images/managers/kevin-sheedy.jpg",
              stringr::str_detect(value, "McAteer") ~ "./www/images/managers/jason-mcateer.jpg"
            ),
            style = dplyr::case_when(
              .default = "height: 50px; border-radius: 50%;",
              value == "No manager" ~ "height: 50px;"
            ),
            alt = value
          )
          tagList(
            div(style = "display: inline-block; width: 60px;", image)
          )
        }),
      mgr_name = reactable::colDef(
        name = "",
        vAlign = "center",
        minWidth = 110
      ),
      P = reactable::colDef(
        vAlign = "center",
        minWidth = 70
      ),
      W = reactable::colDef(
        vAlign = "center",
        minWidth = 70
      ),
      D = reactable::colDef(
        vAlign = "center",
        minWidth = 70
      ),
      L = reactable::colDef(
        vAlign = "center",
        minWidth = 70
      ),
      GF = reactable::colDef(
        vAlign = "center",
        minWidth = 70
      ),
      GA = reactable::colDef(
        vAlign = "center",
        minWidth = 70
      ),
      GD = reactable::colDef(
        vAlign = "center",
        minWidth = 70,
        # Function to add plus sign (+) before positive figures
        cell = function(value) {
          format_gd(value)
        }
      ),
      win_pc = reactable::colDef(
        name = "Win Rate",
        align = "right",
        vAlign = "center",
        minWidth = 150,
        defaultSortOrder = "desc",
        # Render the bar charts using a custom cell render function
        cell = function(value) {
          # Format as percentages with 1 decimal place
          value <- paste0(format(round(value * 100, 1), nsmall = 1), "%")
          bar_chart(
            value,
            width = value,
            fill = "lightblue",
            background = "#F2F2F2"
          )
        }
      )
    )
  )
}
