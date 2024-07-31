# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 2                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Place each of the plots in a `card()` with a header.
#
# 2. What happens when you set `fill = FALSE` or `fillable = FALSE` in a card?
#
# 3. Give each card a **minimum height** to prevent squishing.
#
# 4. How is the plotly plot different from the ggplot2 plot?

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
  plotlyOutput("plot_control"),
  plotOutput("plot_deg_predominant"),
  plotOutput("plot_locale_type")
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