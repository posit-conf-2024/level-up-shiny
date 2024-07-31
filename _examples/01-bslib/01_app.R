library(shiny)
library(bslib)
library(plotly)
library(collegeScorecard)

colors <- c("#007bc2", "#f45100", "#bf007f")

ui <- page_fluid(
  plotlyOutput("plot_control")
)

server <- function(input, output, session) {
  output$plot_control <- renderPlotly({
    plot_school_var(school, "control", title = "School Governance", color = colors[1])
  })

  output$plot_deg_predominant <- renderPlotly({
    plot_school_var(school, "deg_predominant", title = "Predominant Degree", colors[2])
  })

  output$plot_locale_type <- renderPlotly({
    plot_school_var(school, "locale_type", title = "Locale Type", colors[3])
  })
}

plot_school_var <- function(school, var, title = "", color = "blue") {
  school |>
      plot_ly(
        y = ~get(var),
        type = "histogram",
        color = I(color)
      ) |>
      layout(
        title = title,
        xaxis = list(title = "Number of Schools"),
        yaxis = list(title = "")
      ) |>
      config(displayModeBar = FALSE)
}

shinyApp(ui, server)