# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                  Exercise 15.4                   │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(dplyr)
library(dbplyr)
library(ggplot2)
library(leaflet)
library(fontawesome)
library(lgr)

lgr$set_threshold(config::get("log_level"))
lgr$info("Using config", config = Sys.getenv("R_CONFIG_ACTIVE", "default"))

thematic::thematic_shiny()

# Data -----------------------------------------------------------------------
db_env <- here::here("secrets", config::get("db_env"))
if (file.exists(db_env)) {
  dotenv::load_dot_env(db_env)
  lgr$info("Using db.env", db_env = db_env)
}

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = Sys.getenv("DB_DATABASE"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

lgr$info("Connected to database", dbname = Sys.getenv("DB_DATABASE"), user = Sys.getenv("DB_USER"), host = Sys.getenv("DB_HOST"))

school <- tbl(con, "school")
scorecard <- tbl(con, "scorecard")

scorecard_latest <-
  scorecard |>
  group_by(id) |>
  window_order(academic_year) |>
  tidyr::fill(
    n_undergrads,
    rate_admissions,
    rate_completion,
    cost_avg,
    amnt_earnings_med_10y
  ) |>
  slice_max(academic_year, n = 1, with_ties = FALSE) |>
  ungroup()

school_scorecard <-
  school |>
  left_join(scorecard_latest, by = "id")

school_locales <- c("City", "Suburb", "Town", "Rural")

# UI --------------------------------------------------------------------------

ui <- page_sidebar(
  title = config::get("title"),
  class = "bslib-page-dashboard",
  sidebar = sidebar(
    accordion(
      multiple = FALSE,
      accordion_panel(
        title = "Location",
        icon = fa_i("map"),
        selectInput("state", "State", choices = setNames(state.abb, state.name), selected = "WA"),
        checkboxGroupInput("locale_type", "Locale Type", choices = school_locales, selected = school_locales),
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
    input_dark_mode(id = "color_mode")
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
      card_header(
        class = "hstack",
        "Cost vs Earnings",
        popover(
          fa_i("gear", title = "Plot settings", class = "ms-auto"),
          radioButtons(
            "cost_group_by",
            "Group By",
            choices = c(
              "Predominant Degree" = "deg_predominant",
              "Campus Setting" = "locale_type",
              "Testing Requirements" = "adm_req_test"
            ),
          )
        )
      ),
      plotOutput("plot_cost"),
      full_screen = TRUE
    ),
    card(
      class = "text-bg-secondary",
      card_header("School Locations"),
      card_body(
        padding = 0,
        leafletOutput("map")
      ),
      full_screen = TRUE
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

# Server ----------------------------------------------------------------------

server <- function(input, output, session) {
  schools <- reactive({
    school_scorecard |>
      filter(
        state == input$state,
        locale_type %in% input$locale_type,
        between(n_undergrads, !!input$n_undergrads[1], !!input$n_undergrads[2]),
        between(rate_admissions, !!input$rate_admissions[1], !!input$rate_admissions[2]),
        between(rate_completion, !!input$rate_completion[1], !!input$rate_completion[2]),
        between(cost_avg, !!input$cost_avg[1], !!input$cost_avg[2])
      ) |>
      collect()
  })
  
  # Value Boxes ----
  output$vb_public <- renderText({
    schools() |>
      filter(control == "Public") |>
      nrow()
  })
  
  output$vb_nonprofit <- renderText({
    schools() |>
      filter(control == "Nonprofit") |>
      nrow()
  })
  
  output$vb_for_profit <- renderText({
    schools() |>
      filter(control == "For-Profit") |>
      nrow()
  })
  
  # Plots ----
  output$plot_cost <- renderPlot({
    label_dollars <- scales::label_dollar(scale_cut = scales::cut_long_scale())
    
    schools() |>
      ggplot() +
      aes(
        x = cost_avg,
        y = amnt_earnings_med_10y,
        color = !!rlang::sym(input$cost_group_by)
      ) +
      geom_point(size = 5) +
      labs(
        title = NULL,
        x = "Average Cost",
        y = "Median Earnings",
        color = NULL
      ) +
      scale_x_continuous(labels = label_dollars) +
      scale_y_continuous(labels = label_dollars) +
      scale_color_manual(
        values = c(
          "#007bc2",
          "#f45100",
          "#f9b928",
          "#03c7e8",
          "#bf007f",
          "#00891a",
          "#00bf7f"
        )
      ) +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_line()
      )
  })
  
  # Leaflet Map ----
  output$map <- renderLeaflet({
    addColorModeTiles <- function(map) {
      lgr$debug("Rendering map with color mode", color_mode = input$color_mode)
      if (input$color_mode == "light") {
        lgr$debug("Choosing OpenStreetMap.Mapnik tiles")
        addProviderTiles(map, "OpenStreetMap.Mapnik")
      } else if (input$color_mode == "dark") {
        lgr$debug("Choosing CartoDB.DarkMatter tiles")
        addProviderTiles(map, "CartoDB.DarkMatter")
      }
    }
    
    leaflet() |>
      addColorModeTiles() |>
      addMarkers(
        data = schools(),
        lng = ~longitude,
        lat = ~latitude,
        popup = ~name
      )
  })
}

shinyApp(ui, server)