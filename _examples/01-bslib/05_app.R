library(shiny)
library(bslib)
library(glue)
library(dplyr)
library(purrr)
library(collegeScorecard)

ui <- page_fluid(
  class = "p-4",
  sliderInput("n", "Top N Schools", min = 1, max = 20, value = 9, ticks = FALSE),
  uiOutput("layout_school_cards")
)

server <- function(input, output, session) {
  colors <- c("blue", "indigo", "purple", "pink", "red", "orange", "yellow", "green", "teal", "cyan")
  
  output$layout_school_cards <- renderUI({
    school_cards()
  })

  school_cards <- reactive({
    set.seed(42**3.8)

    pmap(top_n_schools(), function(name, cost_avg, city, state, ...) {
      # Turn this into a value box
      p(
        strong(name),
        glue("{city}, {state}")
      )
    })
  })

  top_n_schools <- reactive({
    scorecard |>
      filter(n_undergrads > 1000) |>
      slice_max(academic_year, n = 1) |>
      slice_max(cost_avg, n = input$n) |>
      arrange(desc(cost_avg)) |>
      left_join(school, by = "id")
  })
}

shinyApp(ui, server)
