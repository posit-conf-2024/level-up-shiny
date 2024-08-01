library(shiny)
library(bslib)

ui <- page_fluid(
  card(
    "Undergrad Students",
    5612
  ),
  card(
    "Average Yearly Cost",
    32125
  ),
  card(
    "Completion Rate",
    0.83
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)