library(bslib)
library(shiny)

page_fillable(
  padding = "1rem",
  card(
    card_header("Card Header"),
    p("This is the body of the card"),
    p("Anything here becomes part of the card body."),
    card_footer("Card footer. © bslib 2024")
  )
) |>
  shiny::shinyApp(ui = _, server = \(...) { }) |>
  webshot2::appshot(
    "website/slides/assets/bslib-card.png",
    vwidth = 400,
    vheight = 250,
    zoom = 3
  )

page_fillable(
  padding = "1rem",
  card(
    card_header("Card Header"),
    card_body(
      fillable = FALSE,
      p("This is the body of the card"),
      p("Anything here becomes part of the card body.")
    ),
    card_footer("Card footer. © bslib 2024")
  )
) |>
  shiny::shinyApp(ui = _, server = \(...) { }) |>
  webshot2::appshot(
    "website/slides/assets/bslib-card-not-fillable.png",
    vwidth = 400,
    vheight = 250,
    zoom = 3
  )