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

# Modules --------------------------------------------------------------------
mod_school_ui <- function(id, label, choices, selected = NULL) {
  ns <- NS(id)

  if (is.null(selected)) {
    selected <- sample(choices, 1)
  }

  list(
    selectInput(ns("school"), label, choices = choices, selected = selected, width = "100%"),
    card(
      card_header("Cost of Tuition (In State)"),
      plotOutput(ns("plot_school"))
    )
  )
}

mod_school_server <- function(id, plot_color) {
  moduleServer(id, function(input, output, session) {
    output$plot_school <- renderPlot({
      req(input$school)

      scorecard |>
        filter_scorecard_by_school_name(school, input$school) |>
        plot_cost_tuition(plot_color)
    })
  })
}

# Functions (UI) ------------------------------------------------------------
card_dark <- function(title, ...) {
  card(
    card_header(title),
    class = "text-bg-dark",
    card_body(
      padding = 0,
      ...
    )
  )
}

# UI -------------------------------------------------------------------------

ui <- page_fillable(
  layout_columns(
    div(
      mod_school_ui("a", "School A", school_names),
      card_dark(
        title = "Location",
        leafletOutput("map_school_a")
      )
    ),
    div(
      mod_school_ui("b", "School B", school_names),
      card_dark(
        title = "Location",
        leafletOutput("map_school_b")
      )
    )
  )
)

# Functions ------------------------------------------------------------------

plot_cost_tuition <- function(scorecard, fill_color = "#007bc2") {
  scorecard |>
    mutate(academic_year = as.integer(substr(academic_year, 1, 4))) |>
    ggplot() +
    aes(x = academic_year, y = cost_tuition_in) +
    geom_col(fill = fill_color, na.rm = TRUE) +
    labs(
      x = "Academic Year",
      y = NULL
    ) +
    scale_y_continuous(labels = scales::label_dollar())
}

filter_scorecard_by_school_name <- function(scorecard, school, name) {
  school_by_name <- 
    school |>
    filter(name == {{ name }})

  scorecard |>
    semi_join(school_by_name, by = "id")
}

map_school <- function(school, name) {
  the_school <- school |> filter(name == {{ name }})

  leaflet() |>
    addTiles() |>
    addMarkers(
      lng = the_school$longitude,
      lat = the_school$latitude,
      popup = the_school$name
    )
}

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  mod_school_server("a", colors[1])
  mod_school_server("b", colors[2])

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