# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 4                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Use the Build-A-Box app to design three value boxes
#    * https://bslib.shinyapps.io/build-a-box
#    * shiny::runExample("build-a-box", package = "bslib")
#
# 2. Some icon hints:
#    * public - `fa_i("university")`
#    * non-profit - `fa_i("school-lock")` (it's still private!)
#    * for-profit - `fa_i("building")`

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
  card(
    "Public",
    textOutput("vb_public")
  ),
  card(
    "Nonprofit",
    textOutput("vb_nonprofit")
  ),
  card(
    "For-Profit",
    textOutput("vb_for_profit")
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