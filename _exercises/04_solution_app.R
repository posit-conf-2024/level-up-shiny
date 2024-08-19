# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 4                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

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
  uiOutput("value_box_public", fill = FALSE),
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
  schools <- reactive({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      )
  })
  
  output$value_box_public <- renderUI({
    n_public_schools <- 
      schools() |>
      filter(control == "Public") |>
      nrow()
    
    value_box(
      "Public",
      n_public_schools,
      showcase = fa_i("university"),
      theme = if (n_public_schools > 25) {
        "success"
      } else {
        "danger"
      }
    )
  })

  output$vb_nonprofit <- renderText({
    schools() |>
      filter(control == "Nonprofit") |>
      nrow()
  })

  output$vb_for_profit <- renderText({
    schools() |>
      filter(control == "For-Profit") |>
      nrow()
  })
}

shinyApp(ui, server)