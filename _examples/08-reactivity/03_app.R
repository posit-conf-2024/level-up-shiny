library(shiny)
library(collegeScorecard)
library(bslib)
library(dplyr)
reactlog::reactlog_enable()

# UI
ui <- page_sidebar(
  theme = bs_theme(version = 5),
  title = "College Affordability Analyzer",
  sidebar = sidebar(
    open = list(mobile = "always-above"),
    selectInput("state", "State", choices = setNames(state.abb, state.name), selected = "WA"),
    sliderInput("sat_score", "Minimum SAT Score", min = 800, max = 1200, value = 1000, step = 10),
    input_switch("by_income_bracket", "By Income Bracket", value = TRUE),
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
    textOutput("txt_avg_output"),
    showcase = bsicons::bs_icon("wallet-fill"),
    fill = FALSE
  ),
  value_box(
    "Average 4-year Cost",
    textOutput("txt_four_year_cost"),
    "For the selected income bracket",
    showcase = bsicons::bs_icon("cash-stack"),
    fill = FALSE
  ),
  card(
    reactlog::reactlog_module_ui()
  )
)

# Server
server <- function(input, output, session) {
  reactlog::reactlog_module_server(height = "100%")
  
  filtered_data <- reactive({
    scorecard |>
      filter(
        academic_year == "2020-21",
        !is.na(cost_avg),
        !is.na(amnt_earnings_med_10y)
      ) |>
      select(id, amnt_earnings_med_10y, score_sat_avg, starts_with("cost_avg")) |>
      left_join(school, by = join_by(id)) |>
      filter(
        state == input$state,
        score_sat_avg >= input$sat_score
      )
  })

  observeEvent(input$state, {
    output$txt_avg_output <- renderText({
      filtered_data() |>
        pull(amnt_earnings_med_10y) |>
        mean(na.rm = TRUE) |>
        scales::dollar(accuracy = 10)
    })
  })

  output$txt_four_year_cost <- renderText({
    cost_var <- if (input$by_income_bracket) {
      input$income_bracket
    } else {
      "cost_avg"
    }

    avg_cost <- 
      filtered_data() |>
      pull(!!cost_var) |>
      mean(na.rm = TRUE)

    scales::dollar(avg_cost * 4, accuracy = 10)
  })
}

# Run the app
shinyApp(ui, server)