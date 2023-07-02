value_boxes <- bslib::card(
  class = c("borderless", "no_padding"),
  bslib::card_body(
    class = "no_padding",
    bslib::layout_column_wrap(
      width = 1/5,

      bslib::value_box(
        title = "1st value",
        value = "123",
        showcase = fontawesome::fa("r-project", fill = "steelblue"),
        p("The 1st detail")
      ),
      bslib::value_box(
        title = "2nd value",
        value = "456",
        showcase = bsicons::bs_icon("graph-up"),
        p("The 2nd detail"),
        p("The 3rd detail")
      ),
      bslib::value_box(
        title = "3rd value",
        value = "789",
        showcase = bsicons::bs_icon("pie-chart"),
        p("The 4th detail"),
        p("The 5th detail"),
        p("The 6th detail")
      ),
      bslib::value_box(
        title = "1st value",
        value = "123",
        showcase = bsicons::bs_icon("bar-chart"),
        p("The 1st detail")
      ),
      bslib::value_box(
        title = "2nd value",
        value = "456",
        showcase = bsicons::bs_icon("graph-up"),
        p("The 2nd detail"),
        p("The 3rd detail")
      )
    )
  )
)
