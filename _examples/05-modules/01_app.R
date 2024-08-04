library(shiny)
library(bslib)

badge <- function(text) {
  text
}

ui <- page_fluid(
  h2("Example heading", badge("New")),
  h2("Example heading", badge("Draft")),
  h2("Example heading", badge("Live"))
)

server <- function(input, output, session) {

}

shinyApp(ui, server)