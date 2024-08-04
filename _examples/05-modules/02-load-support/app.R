library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(collegeScorecard)

# Setup ----------------------------------------------------------------------

colors <- c("#007bc2", "#f45100", "#bf007f")

theme_set(
  theme_minimal(18) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(size = 14)
    )
)

school_100 <-
  scorecard |>
  slice_max(academic_year) |>
  slice_max(n_undergrads, n = 100, with_ties = FALSE) |>
  select(id, n_undergrads) |>
  left_join(school, by = join_by(id))

school_names <- c("Pick a School" = "", school_100$name)
school_a <- sample(school_names, 1)
school_b <- sample(school_names, 1)

# UI -------------------------------------------------------------------------

ui <- page_fillable(
  layout_columns(
    div(
      selectInput("school_a", "School A", choices = school_names, selected = school_a, width = "100%"),
      card(
        card_header("Cost of Tuition (In State)"),
        plotOutput("plot_school_a")
      ),
      card_dark(
        title = "Location",
        leafletOutput("map_school_a")
      )
    ),
    div(
      selectInput("school_b", "School B", choices = school_names, selected = school_b, width = "100%"),
      card(
        card_header("Cost of Tuition (In State)"),
        plotOutput("plot_school_b")
      ),
      card_dark(
        title = "Location",
        leafletOutput("map_school_b")
      )
    )
  )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  output$plot_school_a <- renderPlot({
    req(input$school_a)

    scorecard |>
      filter_scorecard_by_school_name(school, input$school_a) |>
      plot_cost_tuition(colors[1])
  })

  output$plot_school_b <- renderPlot({
    req(input$school_b)

    scorecard |>
      filter_scorecard_by_school_name(school, input$school_b) |>
      plot_cost_tuition(colors[2])
  })

  output$map_school_a <- renderLeaflet({
    req(input$school_a)
    map_school(school, input$school_a)
  })

  output$map_school_b <- renderLeaflet({
    req(input$school_b)
    map_school(school, input$school_b)
  })
}

shinyApp(ui, server)