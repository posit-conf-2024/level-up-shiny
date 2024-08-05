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
      mod_school_ui("a", "School A", school_names)
    ),
    div(
      mod_school_ui("b", "School B", school_names)
    )
  )
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  mod_school_server("a", colors[1])
  mod_school_server("b", colors[2])
}

shinyApp(ui, server)