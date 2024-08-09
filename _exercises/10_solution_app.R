# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 10                   │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(collegeScorecard)

colors <- c(
  "Public" = "#007bc2",
  "Nonprofit" = "#f45100",
  "For-profit" = "#bf007f"
)

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Cost of Tuition (In State)",
  sidebar = sidebar(
    selectInput("state", "State", choices = setNames(state.abb, state.name), selected = "NY")
  ),
  useBusyIndicators(),
  busyIndicatorOptions(
    spinner_type = "bars",
    spinner_color = colors["Public"],
    spinner_selector = ".card-cost-public"
  ),
  busyIndicatorOptions(
    spinner_type = "dots",
    spinner_color = colors["Nonprofit"],
    spinner_selector = ".card-cost-nonprofit"
  ),
  busyIndicatorOptions(
    spinner_type = "pulse",
    spinner_color = colors["For-profit"],
    spinner_selector = ".card-cost-for-profit"
  ),
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
    Sys.sleep(4)

    school |>
      filter(state == input$state) |>
      inner_join(scorecard, y = _, by = "id") |>
      filter(!is.na(cost_tuition_in)) |>
      slice_max(academic_year, n = 1, by = id)
  })

  plot_cost_tuition <- function(data, control) {
    range <- range(data$cost_tuition_in, na.rm = TRUE)

    data |>
      filter(control == !!control) |>
      ggplot(aes(x = cost_tuition_in)) +
      geom_histogram(
        position = "identity",
        show.legend = FALSE,
        fill = colors[control]
      ) +
      labs(x = NULL, y = NULL) +
      scale_x_continuous(
        labels = scales::label_dollar(),
        limits = c(floor(range[1] / 1000) * 1000, ceiling(range[2] / 1000) * 1000)
      ) +
      theme_minimal(18)
  }

  output$plot_cost_tuition_public <- renderPlot({
    plot_cost_tuition(schools_state(), "Public")
  })

  output$plot_cost_tuition_nonprofit <- renderPlot({
    plot_cost_tuition(schools_state(), "Nonprofit")
  })

  output$plot_cost_tuition_for_profit <- renderPlot({
    plot_cost_tuition(schools_state(), "For-profit")
  })
}

shinyApp(ui, server)