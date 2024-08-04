library(shiny)
library(bslib)

badge <- function(text, theme = "primary", pill = FALSE, ...) {
  span(
    class = "badge",
    class = paste0("text-bg-", theme),
    class = if (pill) "rounded-pill",
    text,
    ...
  )
}

ui <- page_fluid(
  h2("Example heading", badge("New")),
  h2("Example heading", badge("New", "danger")),
  h2("Example heading", badge("Live", "info", pill = TRUE))
)

server <- function(input, output, session) {

}

shinyApp(ui, server)