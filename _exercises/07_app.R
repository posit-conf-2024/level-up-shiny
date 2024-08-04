# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 7                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Refactor the code for the two plots into a single function.
#    You can put this function in this app file in the server section around the
#    "## Put your function here ##" line.
#
# 2. Replace the the duplicated code with your new function.
#
# 3. What logic is encapsulated in your function? How could your function be
#    used outside of this app? How well will your function compose with other
#    functions?

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
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
      selectInput("school_a", "School A", choices = school_names),
      card(
        card_header("Cost of Tuition (In State)"),
        plotOutput("plot_school_a")
      )
    ),
    div(
      selectInput("school_b", "School B", choices = school_names),
      card(
        card_header("Cost of Tuition (In State)"),
        plotOutput("plot_school_b")
      )
    )
  )
)

# Functions ------------------------------------------------------------------

## Put your function(s) here ##

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  output$plot_school_a <- renderPlot({
    req(input$school_a)

    school_a <- school |> filter(name == input$school_a)

    scorecard |>
      semi_join(school_a, by = "id") |>
      mutate(academic_year = as.integer(substr(academic_year, 1, 4))) |>
      ggplot() +
      aes(x = academic_year, y = cost_tuition_in) +
      geom_col(fill = colors[1], na.rm = TRUE) +
      labs(
        x = "Academic Year",
        y = NULL
      ) +
      scale_y_continuous(labels = scales::label_dollar())
  })

  output$plot_school_b <- renderPlot({
    req(input$school_b)

    school_b <- school |> filter(name == input$school_b)

    scorecard |>
      semi_join(school_b, by = "id") |>
      mutate(academic_year = as.integer(substr(academic_year, 1, 4))) |>
      ggplot() +
      aes(x = academic_year, y = cost_tuition_in) +
      geom_col(fill = colors[2], na.rm = TRUE) +
      labs(
        x = "Academic Year",
        y = NULL
      ) +
      scale_y_continuous(labels = scales::label_dollar())
  })
}

shinyApp(ui, server)