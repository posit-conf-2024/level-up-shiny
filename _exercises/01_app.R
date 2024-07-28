# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Exercise 1                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘
#
# TASKS:
# 1. Run the app and use it to learn about the `school` and `scorecard` datasets.
#
# 2. Load {bslib} and change the theme of the app, using your favorite colors
#    for the background and foreground colors. *Hint:* Check out
#    https://coolors.co for color inspiration.
#
# 3. Choose an accent color for the primary color of the app.
#
# 4. Add `thematic::thematic_shiny()` to the app to make the plots look better.

library(shiny)

ui <- fluidPage(
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

