# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 1                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)

thematic::thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#0B3954",
    fg = "#bfd7ea",
    primary = "#0BAFC1",
  ),
  selectizeInput("data", "Data set", choices = c("school", "scorecard"), selected = "school"),
  radioButtons("type", "Inspection type", choices = c("Column Types" = "types", "Categorical" = "cat", "Numeric" = "num", "Missing" = "na"), inline = TRUE),
  plotOutput("plot")
)

server <- function(input, output, session) {
  data <- reactive({
    switch(
      input$data,
      "school" = collegeScorecard::school,
      "scorecard" = collegeScorecard::scorecard
    )
  })

  output$plot <- renderPlot({
    req(data())

    df <- data()

    inspected <- switch(
      input$type,
      "types" = inspectdf::inspect_types(df),
      "cat" = inspectdf::inspect_cat(df),
      "num" = inspectdf::inspect_num(df),
      "na" = inspectdf::inspect_na(df)
    )

    inspectdf::show_plot(inspected, col_palette = 2)
  })
}

shinyApp(ui, server)

