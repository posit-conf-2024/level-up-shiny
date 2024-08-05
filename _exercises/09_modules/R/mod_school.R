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