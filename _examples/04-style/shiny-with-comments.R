# Load the Shiny library
library(shiny)

# Define the user interface for the application
ui <- fluidPage(
  
  # Create a title for the application
  titlePanel("My Shiny App"), 
  
  # Create a sidebar layout
  sidebarLayout(
    
    # Define the sidebar panel
    sidebarPanel(
      # Create a numeric input field
      numericInput("num", "Enter a number:", value = 10) 
    ),
    
    # Define the main panel
    mainPanel(
      # Output the result as a text
      textOutput("result") 
    )
  )
)

# Define the server logic for the application
server <- function(input, output) {
  
  # Create a reactive expression to calculate the square of the input
  output$result <- renderText({
    # Calculate the square of the input number
    input$num^2 
  })
}

# Run the application
shinyApp(ui = ui, server = server)
