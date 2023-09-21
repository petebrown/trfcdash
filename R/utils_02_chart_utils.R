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
