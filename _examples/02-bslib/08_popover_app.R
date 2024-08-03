library(shiny)
library(bslib)
library(fontawesome)

ui <- page_fillable(
  class = "justify-content-center align-items-center",
  popover(
    fontawesome::fa_i("gear", title = "Settings"),
    title = "Plot settings",
    "I'm the popover content."
  ),
  card(
    card_header(
      class = "hstack",
      "Card Title",
      popover(
        fontawesome::fa_i("gear", title = "Settings", class = "ms-auto"),
        title = "Plot settings",
        input_switch("show_legend", "Show legend", TRUE)
      )
    ),
    "A map or a plot would go here.",
    max_height = 300
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)