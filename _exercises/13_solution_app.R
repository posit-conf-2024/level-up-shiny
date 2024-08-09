# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 13                   │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(collegeScorecard)

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Cost of Tuition (In State)",
  sidebar = sidebar(
    selectInput("state", "State", choices = setNames(state.abb, state.name), selected = "NY"),
    actionButton("get_state", "Get State"),
    hr(class = "m-0"),
    sliderInput("tuition_range", "Tuition Range", min = 0, max = 50000, value = c(0, 50000), ticks = FALSE)
  ),
  useBusyIndicators(),
  card(
    class = "card-cost-public",
    card_header("Public"),
    plotOutput("plot_cost_tuition_public")
  ),
  card(
    class = "card-cost-nonprofit",
    card_header("Nonprofit"),
    plotOutput("plot_cost_tuition_nonprofit")
  ),
  card(
    class = "card-cost-for-profit",
    card_header("For-profit"),
    plotOutput("plot_cost_tuition_for_profit")
  )
)

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  schools_state <- reactive({
    fetch_college_data(input$state)
  }) |>
    bindEvent(input$get_state)

  # Plots ----
  output$plot_cost_tuition_public <- renderPlot({
    req(schools_state())

    schools_state() |>
      plot_cost_tuition("Public", input$tuition_range)
  })

  output$plot_cost_tuition_nonprofit <- renderPlot({
    req(schools_state())

    schools_state() |>
      plot_cost_tuition("Nonprofit", input$tuition_range)
  })

  output$plot_cost_tuition_for_profit <- renderPlot({
    req(schools_state())

    schools_state() |>
      plot_cost_tuition("For-profit", input$tuition_range)
  })
}

# Support ---------------------------------------------------------------------

colors <- c(
  "Public" = "#007bc2",
  "Nonprofit" = "#f45100",
  "For-profit" = "#bf007f"
)

plot_cost_tuition <- function(data, control, tuition_range) {
  data |>
    filter(control == !!control) |>
    filter(between(cost_tuition_in, tuition_range[1], tuition_range[2])) |>
    ggplot(aes(x = cost_tuition_in)) +
    geom_histogram(
      position = "identity",
      show.legend = FALSE,
      fill = colors[control]
    ) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(
      labels = scales::label_dollar(),
      limits = tuition_range
    ) +
    theme_minimal(18)
}

fetch_college_data <- function(state, tuition_range) {
  # Pretend we're fetching the data from an API ;)
  Sys.sleep(4)

  school |>
    filter(state == !!state) |>
    inner_join(scorecard, y = _, by = "id") |>
    filter(!is.na(cost_tuition_in)) |>
    slice_max(academic_year, n = 1, by = id)
}



shinyApp(ui, server)