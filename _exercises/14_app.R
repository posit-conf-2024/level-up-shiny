# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 14                   │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Replace the `actionButton("get_state")` with an `input_task_button()`.
#    Run the app at this point. How much does the task button help?
# 
# 2. Update the server logic to use `ExtendedTask`. Don't forget to wrap the
#    "API call" in a `future_promise()`. I've already loaded the packages you
#    need.
#
#    Here's the boilerplate example we saw in the slides:
#
#    ```r
#    task_fetch_data <- ExtendedTask$new(function(data_type) {
#      future_promise({
#        fetch_from_slow_api(data_type)
#      })
#    }) |>
#      bind_task_button("fetch_data")
#    
#    observeEvent(input$fetch_data, {
#      task_fetch_data$invoke(input$data_type)
#    })
#    
#    data <- reactive({
#      task_fetch_data$result()
#    })
#    ```
#
# 3. Can you use the app while fetching data from the API in the background?
#    
# 4. Stretch: How could you keep the whole app running, including the plots
#    while the data is being fetched? Read `14_solution2_app.R` for an example.

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
    # CHANGE THIS TO AN INPUT TASK BUTTON
    actionButton("get_state", "Get State"),
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
  # UPDATE THIS LOGIC TO USE AN EXTENDED TASK
  schools_state <- reactive({
    fetch_college_data(input$state)
  }) |>
    bindEvent(input$get_state)

  # Text Outputs ----
  output$txt_state <- renderText({
    paste("State: ", input$state)
  }) |>
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