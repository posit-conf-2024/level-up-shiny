library(shiny)
library(bslib)
library(fontawesome)

ui <- page_fluid(
  value_box(
    "Undergrad Students",
    scales::number(5612, big.mark = ","),
    showcase = fa_i("people-roof")
  ),
  value_box(
    "Average Yearly Cost",
    scales::dollar(32125),
    showcase = fa_i("money-check-dollar"),
    theme = "primary"
  ),
  value_box(
    "Completion Rate",
    scales::percent(0.83),
    showcase = fa_i("user-graduate"),
    theme = "bg-gradient-orange-red"
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)