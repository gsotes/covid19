library(shiny)

source("r_main_analysis.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Report COVID-19 Cases Per Country"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "input_country",
        label = "Select country",
        choices = list(list = countries),
        selected = "Philippines"
      )
      ),
    mainPanel("main panel")
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
