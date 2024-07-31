library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(fontawesome)
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

thematic::thematic_shiny()

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


ui <- page_sidebar(
  title = "Find a School",
  class = "bslib-page-dashboard",
  sidebar = sidebar(
    accordion(
      multiple = FALSE,
      accordion_panel(
        title = "Location",
        icon = fa_i("map"),
        selectInput("state", "State", choices = setNames(state.abb, state.name)),
        checkboxGroupInput("locale_type", "Locale Type", choices = levels(school$locale_type), selected = levels(school$locale_type)),
      ),
      accordion_panel(
        title = "Student Population",
        icon = fa_i("users"),
        sliderInput("n_undergrads", "Number of Undergrads", min = 0, max = 50000, value = c(0, 50000), step = 1000),
      ),
      accordion_panel(
        title = "Admissions",
        icon = fa_i("graduation-cap"),
        sliderInput("rate_admissions", "Admissions Rate", min = 0, max = 1, value = c(0, 1), step = 0.1),
        sliderInput("rate_completion", "Completion Rate", min = 0, max = 1, value = c(0, 1), step = 0.1),
      ),
      accordion_panel(
        title = "Cost",
        icon = fa_i("money-check-dollar"),
        sliderInput("cost_avg", "Average Cost", min = 0, max = 50000, value = c(0, 50000), step = 1000)
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
        "Non-Profit",
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
    navset_card_pill(
      nav_panel(
        "Cost vs Earnings",
        layout_sidebar(
          sidebar = sidebar(
            position = "right",
            open = "closed",
            radioButtons(
              "cost_group_by",
              "Group By",
              choices = c(
                "Predominant Degree" = "deg_predominant",
                "Campus Setting" = "locale_type",
                "Testing Requirements" = "adm_req_test"
              ),
            )
          ),
          plotOutput("plot_cost"),
        ),
        full_screen = TRUE
      ),
      nav_panel(
        "Predominant Degree",
        plotOutput("plot_deg_predominant"),
        full_screen = TRUE
      ),
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
  output$plot_deg_predominant <- renderPlot({
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
      aes(y = deg_predominant) +
      geom_bar(fill = colors[2], na.rm = TRUE) +
      labs(
        title = NULL,
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