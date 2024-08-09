# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 12                   │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(collegeScorecard)

options(fetch_local = FALSE)

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  theme = bs_theme(version = 5),
  title = "College Affordability Analyzer",
  fill = FALSE,
  sidebar = sidebar(
    open = list(mobile = "always-above"),
    selectInput("state", "State", choices = setNames(state.abb, state.name), selected = "NY"),
    sliderInput("sat_score", "Minimum SAT Score", min = 800, max = 1200, value = 1000, step = 10),
    radioButtons(
      "income_bracket",
      "Income Bracket",
      inline = TRUE,
      choices = c(
        "< $30k" = "cost_avg_income_0_30k",
        "$30 - $48k" = "cost_avg_income_30_48k",
        "$48 - $75k" = "cost_avg_income_48_75k",
        "$75 - $110k" = "cost_avg_income_75_110k",
        "$110k+" = "cost_avg_income_110k_plus"
      )
    )
  ),
  value_box(
    "Average 10-year Median Earnings",
    textOutput("txt_amnt_earnings"),
    theme = "bg-gradient-blue-indigo",
    showcase = plotlyOutput("plot_amnt_earnings"),
    showcase_layout = "bottom",
    full_screen = TRUE
  ),
  value_box(
    "Average 4-year Cost",
    textOutput("txt_four_year_cost"),
    "For the selected income bracket",
    theme = "bg-gradient-blue-indigo",
    showcase = plotlyOutput("plot_four_year_cost"),
    showcase_layout = "bottom",
    full_screen = TRUE
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  schools_state <- reactive({
    get_schools_in_state(input$state)
  }) |>
    bindCache(input$state)

  schools_state_with_sat <- reactive({
    schools_state() |>
      filter(score_sat_avg >= input$sat_score)
  })

  output$txt_amnt_earnings <- renderText({
    schools_state_with_sat() |>
      pull(amnt_earnings_med_10y) |>
      mean(na.rm = TRUE) |>
      scales::dollar(accuracy = 10)
  })

  output$txt_four_year_cost <- renderText({
    avg_cost <-
      schools_state_with_sat() |>
      pull(!!input$income_bracket) |>
      mean(na.rm = TRUE)

    scales::dollar(avg_cost * 4, accuracy = 10)
  })

  output$plot_amnt_earnings <- renderPlotly({
    schools_state_with_sat() |>
      plotly_expandable_histogram(
        "amnt_earnings_med_10y",
        font_color = "#FFFFFF80",
        x_title = "Earnings"
      )
  })

  output$plot_four_year_cost <- renderPlotly({
    schools_state_with_sat() |>
      plotly_expandable_histogram(
        input$income_bracket,
        font_color = "white",
        x_title = "Cost"
      )
  })
}


shinyApp(ui, server)
