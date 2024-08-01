library(shiny)
library(bslib)
library(fontawesome)
library(collegeScorecard)

ui <- page_fillable(
  selectInput("state", "State", choices = setNames(state.abb, state.name)),
  checkboxGroupInput("locale_type", "Locale Type", choices = levels(school$locale_type), selected = levels(school$locale_type)),
  sliderInput("n_undergrads", "Number of Undergrads", min = 0, max = 50000, value = c(0, 50000), step = 1000)
)

server <- function(input, output, session) {

}

shinyApp(ui, server)