# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 5                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Use `layout_columns()` and `layout_column_wrap()` to improve the layout of
#    the app.
# 
# 2. Some hints:
#     * Which items should be grouped together in a row?
#     * `layout_columns()` has `col_widths` which takes a vector of column
#       widths in Bootstrap's grid units.
#     * `layout_column_wrap()` has `width` and can take fractional widths, e.g.
#       `1 / 2`.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(fontawesome)
library(collegeScorecard)

# UI -------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Find a School",
  sidebar = sidebar(
    selectInput("state", "State", choices = setNames(state.abb, state.name)),
    checkboxGroupInput("locale_type", "Locale Type", choices = levels(school$locale_type), selected = levels(school$locale_type)),
  ),

  # > Value boxes ----
  value_box(
    "Public",
    textOutput("vb_public"),
    showcase = fa_i("university")
  ),
  value_box(
    "Nonprofit",
    textOutput("vb_nonprofit"),
    theme = "primary",
    showcase = fa_i("school-lock")
  ),
  value_box(
    "For-Profit",
    textOutput("vb_for_profit"),
    theme = "bg-gradient-orange-red",
    showcase = fa_i("building")
  ),

  # > Card: Cost vs Earnings ----
  card(
    card_header("Cost vs Earnings"),
    layout_sidebar(
      sidebar = sidebar(
        open = FALSE,
        position = "right",
        radioButtons(
          "cost_group_by",
          "Group By",
          choices = c(
            "Predominant Degree" = "deg_predominant",
            "Campus Setting" = "locale_type",
            "Testing Requirements" = "adm_req_test"
          ),
        ),
      ),
      plotOutput("plot_cost"),
    ),
    full_screen = TRUE
  ),

  # > Card: Map ----
  card(
    class = "text-bg-secondary",
    card_header("School Locations"),
    card_body(
      padding = 0,
      leafletOutput("map")
    )
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
  # > Value boxes ----
  output$vb_public <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      filter(control == "Public") |>
      nrow()
  })

  output$vb_nonprofit <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      filter(control == "Nonprofit") |>
      nrow()
  })

  output$vb_for_profit <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      filter(control == "For-Profit") |>
      nrow()
  })

  # > Card: Cost vs Earnings ----
  output$plot_cost <- renderPlot({
    label_dollars <- scales::label_dollar(scale_cut = scales::cut_long_scale())

    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      ) |>
      ggplot() +
      aes(
        x = cost_avg,
        y = amnt_earnings_med_10y,
        color = !!rlang::sym(input$cost_group_by)
      ) +
      geom_point(size = 3) +
      labs(
        title = NULL,
        x = "Average Cost",
        y = "Median Earnings",
        color = NULL
      ) +
      scale_x_continuous(labels = label_dollars) +
      scale_y_continuous(labels = label_dollars) +
      scale_color_manual(
        values = c("#007bc2", "#f45100", "#00891a", "#bf007f", "#f9b928", "#03c7e8", "#00bf7f")
      ) +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_line()
      )
  })

  # > Card: Map ----
  output$map <- renderLeaflet({
    school_filtered <-
      school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type
      )

    leaflet() |>
      addTiles() |>
      addMarkers(
        data = school_filtered,
        lng = ~longitude,
        lat = ~latitude,
        popup = ~name
      )
  })
}

shinyApp(ui, server)