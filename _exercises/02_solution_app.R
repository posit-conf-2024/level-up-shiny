# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 2                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(collegeScorecard)

colors <- c("#007bc2", "#f45100", "#bf007f")

theme_set(
  theme_minimal(18) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_text(size = 14)
  )
)

# UI -------------------------------------------------------------------------

ui <- page_fillable(
  card(
    card_header("School Governance"),
    plotlyOutput("plot_control"),
    full_screen = TRUE
  ),
  card(
    card_header("Predominant Degree"),
    plotOutput("plot_deg_predominant"),
    full_screen = TRUE
  ),
  card(
    card_header("Locale Type"),
    plotOutput("plot_locale_type"),
    full_screen = TRUE
  )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  output$plot_control <- renderPlotly({
    school |>
      plot_ly(
        y = ~control,
        type = "histogram",
        color = I(colors[1])
      ) |>
      layout(
        title =  "School Governance",
        xaxis = list(title = "Number of Schools"),
        yaxis = list(title = "")
      ) |>
      config(displayModeBar = FALSE)
  })

  output$plot_deg_predominant <- renderPlot({
    ggplot(school) +
      aes(y = deg_predominant) +
      geom_bar(fill = colors[2], na.rm = TRUE) +
      labs(
        title = "Predominant Degree",
        x = "Number of Schools",
        y = NULL
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(
        labels = \(x) ifelse(is.na(x), "Unknown", x)
      )
  })
  
  output$plot_locale_type <- renderPlot({
    ggplot(school) +
      aes(y = locale_type) +
      geom_bar(fill = colors[3], na.rm = TRUE) +
      labs(
        title = "Locale Type",
        x = "Number of Schools",
        y = NULL
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_discrete(
        labels = \(x) ifelse(is.na(x), "Unknown", x)
      )
  })
}

shinyApp(ui, server)