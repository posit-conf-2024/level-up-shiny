library(shiny)
library(bslib)
library(dplyr)
library(fontawesome)
library(collegeScorecard)

# UI -------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Find a School",
  sidebar = sidebar(
    selectInput("state", "State", choices = setNames(state.abb, state.name)),
    checkboxGroupInput("locale_type", "Locale Type", choices = levels(school$locale_type), selected = levels(school$locale_type)),
  ),
  value_box(
    "Public",
    textOutput("vb_public"),
    showcase = fa_i("university")
  ),
  value_box(
    "Nonprofit",
    textOutput("vb_nonprofit"),
    theme = "primary",
    showcase = fa_i("school-lock")
  ),
  value_box(
    "For-Profit",
    textOutput("vb_for_profit"),
    theme = "bg-gradient-orange-red",
    showcase = fa_i("building")
  )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  output$vb_public <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      filter(control == "Public") |>
      nrow()
  })

  output$vb_nonprofit <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      filter(control == "Nonprofit") |>
      nrow()
  })

  output$vb_for_profit <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      filter(control == "For-Profit") |>
      nrow()
  })
}

shinyApp(ui, server)