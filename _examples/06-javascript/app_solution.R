# ┌ level-up-shiny ──────────────────────────────────┐
# │                                                  │
# │                    Solution 9                    │
# │                                                  │
# └─────────────────────────────── posit::conf(2024) ┘

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(collegeScorecard)

# Setup ----------------------------------------------------------------------

colors <- c("#007bc2", "#f45100", "#bf007f")

theme_set(
  theme_minimal(18) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(size = 14)
    )
)

school_100 <-
  scorecard |>
  slice_max(academic_year) |>
  slice_max(n_undergrads, n = 100, with_ties = FALSE) |>
  select(id, n_undergrads) |>
  left_join(school, by = join_by(id))

school_names <- c("Pick a School" = "", school_100$name)

# UI -------------------------------------------------------------------------

ui <- page_fillable(
  selectInput("state", "State", choices = state.abb),
  actionButton("save", "Save"),
  tags$head(
    tags$script(HTML(
      "
      // Store a variable in local storage when the user inputs a value
      Shiny.addCustomMessageHandler('do_store_value', function(message) {
        localStorage.setItem('the_stored_value', message.value);
      });

      // Retrieve the stored value from local storage using initializedPromise
      Shiny.initializedPromise.then(function() {
        var storedValue = localStorage.getItem('the_stored_value');
        if (storedValue !== null) {
          Shiny.setInputValue('the_stored_value', storedValue);
        }
      });
      "
    ))
  ),
)

# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  observe({
    message("the_stored_value is ", input$the_stored_value)
    updateSelectInput(session, "state", selected = input$the_stored_value)
  })
  
  observeEvent(input$save, {
    session$sendCustomMessage("do_store_value", list(value = input$state))
  })
}

shinyApp(ui, server)