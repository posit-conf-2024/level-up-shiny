library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(collegeScorecard)

options(fetch_local = FALSE)
reactlog::reactlog_enable()


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
  ),
  navset_card_tab(
    nav_panel("Reactlog", reactlog::reactlog_module_ui()),
    nav_panel("Profvis", profvis::profvis_ui("profvis"))
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  reactlog::reactlog_module_server()
  moduleServer("profvis", profvis::profvis_server)

  output$txt_amnt_earnings <- renderText({
    get_schools_in_state_with_sat(input$state, input$sat_score) |>
      pull(amnt_earnings_med_10y) |>
      mean(na.rm = TRUE) |>
      scales::dollar(accuracy = 10)
  })

  output$txt_four_year_cost <- renderText({
    avg_cost <-
      get_schools_in_state_with_sat(input$state, input$sat_score) |>
      pull(!!input$income_bracket) |>
      mean(na.rm = TRUE)

    scales::dollar(avg_cost * 4, accuracy = 10)
  })

  output$plot_amnt_earnings <- renderPlotly({
    get_schools_in_state_with_sat(input$state, input$sat_score) |>
      plotly_expandable_histogram(
        "amnt_earnings_med_10y",
        font_color = "#FFFFFF80",
        x_title = "Earnings"
      )
  })

  output$plot_four_year_cost <- renderPlotly({
    get_schools_in_state_with_sat(input$state, input$sat_score) |>
      plotly_expandable_histogram(
        input$income_bracket,
        font_color = "white",
        x_title = "Cost"
      )
  })
}


shinyApp(ui, server)
