# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 9                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Add the school map card to the `mod_school` ui/server functions.
#    You can find the module in `R/mod_school.R`. Having the module in a 
#    separate file makes it easier to have both the app and the module
#    open at the same time.

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