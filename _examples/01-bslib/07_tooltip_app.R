library(shiny)
library(bslib)

ui <- page_fillable(
  class = "justify-content-center align-items-center",
  tooltip(
    fontawesome::fa_i("info-circle"),
    "Hover over me for more info!"
  ),
  textInput(
    "package", 
    tagList(
      "Package Name",
      tooltip(
        fontawesome::fa_i("info-circle"),
        "Please pick Shiny!"
      ) 
    ),
    placeholder = "e.g. shiny"
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)