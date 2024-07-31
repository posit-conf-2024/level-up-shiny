library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(collegeScorecard)

# UI -------------------------------------------------------------------------

ui <- page_fillable(
  selectInput("state", "State", choices = setNames(state.abb, state.name)),
  card(
    card_header("Predominant Degree"),
    plotOutput("plot_deg_predominant"),
    full_screen = TRUE
  ),
  card(
    card_header("Cost vs Earnings"),
    plotOutput("plot_cost"),
    full_screen = TRUE
  )
)

# Setup -----------------------------------------------------------------------

colors <- c("#007bc2", "#f45100", "#bf007f")

theme_set(
  theme_minimal(18) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(size = 14)
    )
)

scorecard_latest <-
  scorecard |>
  group_by(id) |>
  arrange(academic_year) |>
  tidyr::fill(
    n_undergrads,
    rate_admissions,
    rate_completion,
    cost_avg,
    amnt_earnings_med_10y
  ) |>
  slice_max(academic_year, n = 1, with_ties = FALSE) |>
  ungroup()

school <-
  school |>
  left_join(scorecard_latest, by = "id")

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  output$plot_deg_predominant <- renderPlot({
    school |>
      filter(state == input$state) |>
      ggplot() +
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

  output$plot_cost <- renderPlot({
    label_dollars <- scales::label_dollar(scale_cut = scales::cut_long_scale())

    school |>
      filter(state == input$state) |>
      ggplot() +
      aes(
        x = cost_avg,
        y = amnt_earnings_med_10y,
        # color = !!rlang::sym(input$cost_group_by)
      ) +
      geom_point(size = 3, color = colors[1]) +
      labs(
        title = NULL,
        x = "Average Cost",
        y = "Median Earnings",
        color = NULL
      ) +
      scale_x_continuous(labels = label_dollars) +
      scale_y_continuous(labels = label_dollars) +
      # scale_color_manual(
      #   values = c("#007bc2", "#f45100", "#00891a", "#bf007f", "#f9b928", "#03c7e8", "#00bf7f")
      # ) +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_line()
      )
  })
}

shinyApp(ui, server)