library(shiny)
library(bslib)
library(fontawesome)
library(collegeScorecard)

ui <- page_fillable(
  accordion(
    multiple = FALSE,
    accordion_panel(
      title = "Location",
      icon = fa_i("map"),
      selectInput("state", "State", choices = setNames(state.abb, state.name)),
      checkboxGroupInput("locale_type", "Locale Type", choices = levels(school$locale_type), selected = levels(school$locale_type)),
    ),
    accordion_panel(
      title = "Student Population",
      icon = fa_i("users"),
      sliderInput("n_undergrads", "Number of Undergrads", min = 0, max = 50000, value = c(0, 50000), step = 1000),
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)