# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 14                   │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(collegeScorecard)

library(future)
library(promises)
future::plan(multisession)

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Cost of Tuition (In State)",

  sidebar = sidebar(
    selectInput("state", "State", choices = setNames(state.abb, state.name), selected = "NY"),
    input_task_button("get_state", "Get State"),
    hr(class = "m-0"),
    sliderInput("tuition_range", "Tuition Range", min = 0, max = 50000, value = c(0, 50000), ticks = FALSE)
  ),
  
  useBusyIndicators(),
  
  # | Text Outputs ----
  h3(textOutput("txt_state")),
  uiOutput("ui_tuition_range"),
  
  # | Cards ----
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
  fetch_schools_state <- ExtendedTask$new(function(state) {
    future_promise(
      fetch_college_data(state)
    )
  }) |>
    bind_task_button("get_state")

  observeEvent(input$get_state, {
    # Set the state reactive variable to remember which state was selected
    state(input$state)

    # And then invoke the extended task
    fetch_schools_state$invoke(input$state)
  })

  # Store the fetched data/state in a reactive value so we can still use
  # them in the app while the task runs.
  schools_state <- reactiveVal(NULL)
  state <- reactiveVal(NULL)

  observe({
    # When the task completes, update the schools_state reactive value
    # with the fetched data.
    schools_state(fetch_schools_state$result())
  })

  # Text Outputs ----
  output$txt_state <- renderText({
    # Use the state name from the last extended task run
    paste("State: ", state())
  }) |>
    # ...so only update when the schools data is updated by the task
    bindEvent(schools_state())

  output$ui_tuition_range <- renderUI({
    p(
      "Showing tuition costs between",
      scales::dollar(input$tuition_range[1]),
      "and",
      scales::dollar(input$tuition_range[2])
    )
  })

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

fetch_college_data <- function(state) {
  # Pretend we're fetching the data from an API ;)
  Sys.sleep(4)

  school |>
    filter(state == !!state) |>
    inner_join(scorecard, y = _, by = "id") |>
    filter(!is.na(cost_tuition_in)) |>
    slice_max(academic_year, n = 1, by = id)
}



shinyApp(ui, server)