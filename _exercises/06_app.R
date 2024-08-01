# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 6                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Use `accordion()` and `accordion_panel()` to organize the sidebar inputs.
#    For {fontawesome} use https://fontawesome.com/search?m=free to find icons.
#    Or for {bsicons} use https://icons.getbootstrap.com/ to find icons.
#
# 2. Add informational tooltips to the value box titles.
#    * Public: "Supported by public funds and operated by elected or appointed officials."
#    * Nonprofit: "Private institutions that are not operated for profit."
#    * For-Profit: "Operated by private, profit-seeking businesses."
#
# 3. Stretch: Replace the local sidebar with a popover element.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(fontawesome)
library(collegeScorecard)

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  title = "Find a School",
  class = "bslib-page-dashboard",
  sidebar = sidebar(
    selectInput("state", "State", choices = setNames(state.abb, state.name)),
    checkboxGroupInput("locale_type", "Locale Type", choices = levels(school$locale_type), selected = levels(school$locale_type)),
    sliderInput("n_undergrads", "Number of Undergrads", min = 0, max = 50000, value = c(0, 50000), step = 1000),
    sliderInput("rate_admissions", "Admissions Rate", min = 0, max = 1, value = c(0, 1), step = 0.1),
    sliderInput("rate_completion", "Completion Rate", min = 0, max = 1, value = c(0, 1), step = 0.1),
    sliderInput("cost_avg", "Average Cost", min = 0, max = 50000, value = c(0, 50000), step = 1000)
    accordion(
      multiple = FALSE,
      accordion_panel(
        title = "Location",
        icon = fa_i("map"),
      ),
      accordion_panel(
        title = "Student Population",
        icon = fa_i("users"),
      ),
      accordion_panel(
        title = "Admissions",
        icon = fa_i("graduation-cap"),
      ),
      accordion_panel(
        title = "Cost",
        icon = fa_i("money-check-dollar"),
      )
    ),
    input_dark_mode()
  ),
  layout_column_wrap(
    width = 1 / 3,
    fill = FALSE,
    value_box(
      span(
        "Public",
        tooltip(
          fa_i("info-circle"),
          "Supported by public funds and operated by elected or appointed officials."
        )
      ),
      textOutput("vb_public"),
      showcase = fa_i("university")
    ),
    value_box(
      span(
        "Nonprofit",
        tooltip(
          fa_i("info-circle"),
          "Private institutions that are not operated for profit."
        )
      ),
      textOutput("vb_nonprofit"),
      theme = "primary",
      showcase = fa_i("school-lock")
    ),
    value_box(
      span(
        "For-Profit",
        tooltip(
          fa_i("info-circle"),
          "Operated by private, profit-seeking businesses."
        )
      ),
      textOutput("vb_for_profit"),
      theme = "bg-gradient-orange-red",
      showcase = fa_i("building")
    )
  ),
  layout_columns(
    col_widths = c(8, 4),
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
    card(
      class = "text-bg-secondary",
      card_header("School Locations"),
      card_body(
        padding = 0,
        leafletOutput("map")
      )
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

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  # Value Boxes ----
  output$vb_public <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type,
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(rate_admissions, input$rate_admissions[1], input$rate_admissions[2]),
        between(rate_completion, input$rate_completion[1], input$rate_completion[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
      ) |>
      filter(control == "Public") |>
      nrow()
  })

  output$vb_nonprofit <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type,
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(rate_admissions, input$rate_admissions[1], input$rate_admissions[2]),
        between(rate_completion, input$rate_completion[1], input$rate_completion[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
      ) |>
      filter(control == "Nonprofit") |>
      nrow()
  })

  output$vb_for_profit <- renderText({
    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type,
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(rate_admissions, input$rate_admissions[1], input$rate_admissions[2]),
        between(rate_completion, input$rate_completion[1], input$rate_completion[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
      ) |>
      filter(control == "For-Profit") |>
      nrow()
  })

  # Plots ----
  output$plot_cost <- renderPlot({
    label_dollars <- scales::label_dollar(scale_cut = scales::cut_long_scale())

    school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type,
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(rate_admissions, input$rate_admissions[1], input$rate_admissions[2]),
        between(rate_completion, input$rate_completion[1], input$rate_completion[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
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

  # Leaflet Map ----
  output$map <- renderLeaflet({
    school_filtered <-
      school |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type,
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(rate_admissions, input$rate_admissions[1], input$rate_admissions[2]),
        between(rate_completion, input$rate_completion[1], input$rate_completion[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
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