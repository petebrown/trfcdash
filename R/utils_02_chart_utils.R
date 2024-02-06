#' 02_chart_utils
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

facet_wrap_theme <- function() {
  ggplot2::theme(
    legend.position = "bottom",
    text = ggtext::element_markdown(
      family = "Helvetica Neue",
      face = "plain",
      size = 16,
      colour = NULL,
      fill = NA,
      box.colour = NA,
      linetype = NA,
      linewidth = NA,
      hjust = NULL,
      vjust = NULL,
      halign = "right",
      valign = NA,
      angle = NULL,
      lineheight = NULL,
      margin = NULL,
      padding = NA,
      r = NA,
      align_widths = NA,
      align_heights = NA,
      rotate_margins = NA,
    ),
    strip.text.x = ggplot2::element_text(
      hjust = 0,
      size = 20,
      color = "#4c668d",
      face = "bold",
      margin = ggplot2::margin(
        t = 0,
        b = 15,
        r = 0,
        l = 0,
        "pt"
      )
    ),
    strip.background = ggplot2::element_rect(
      fill = "transparent",
      color="transparent"
    ),
    panel.border = ggplot2::element_blank(),
    panel.spacing = ggplot2::unit(2, "lines"),
    line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      color = "lightgrey"
    ),
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2:: element_blank(),
    panel.grid.major.x = ggplot2::element_line(
      color = "lightgrey",
      size = 0.20
    ),
    panel.background = ggplot2::element_rect(
      fill = "transparent"
    ),
    plot.background = ggplot2::element_rect(
      fill = "transparent", color = NA
    ),
    legend.box.background = ggplot2::element_rect(
      fill = "transparent",
      color = "transparent",
      linewidth = 0
    ),
    legend.box.spacing = ggplot2::unit(15, "pt")
  )
}


# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "1.2rem", fill = "#00bfc4", background = NULL) {
  bar <- div(
    style = list(
      background = fill,
      width = width,
      height = height
    )
  )

  chart <- div(
    style = list(
      flexGrow = 1,
      marginLeft = "10%",
      marginRight = "1.5rem",
      background = background,
      `border-style` = "solid",
      `border-color` = "slategrey",
      `border-width` = "thin"
    ),
    bar
  )

  label_div <- div(
    style = list(
      width = "55px"
    ),
    label
  )

  # Output div containing chart and label
  div(
    style = list(
      display = "flex",
      alignItems = "center"
    ),
    chart,
    label_div
  )
}

wdl_chart <- function(win_pc, draw_pc, loss_pc) {
  bar <- function(value, background_color) {
    div(
      style = list(
        flex = value,
        `background-color` = background_color,
        width = value,
        height = "1.2rem",
        `text-align` = "center",
        `border-style` = "solid",
        `border-color` = "white",
        `border-width` = "thin"
      )
    )
  }

  chart <- div(
    style = list(
      display = "flex",
      marginLeft = "10%",
      marginRight = "1.5rem",
      background = NULL,
      `border-style` = "solid",
      `border-color` = "black",
      `border-width` = "thin"
    ),
    bar(win_pc, background_color="darkgreen"),
    bar(draw_pc, background_color="grey"),
    bar(loss_pc, background_color="red")
  )

  # Output div containing chart and label
  div(
    style = list(
      display = "flex",
      alignItems = "center"
    ),
    chart
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
