library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Explore the AirQuality Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Choose Month:", 
                  choices = c("May" = 5, "June" = 6, "July" = 7, "August" = 8, "September" = 9),
                  selected = 5),
      selectInput("yvar", "Variable to Plot:", 
                  choices = c("Ozone", "Temp", "Solar.R", "Wind")),
      checkboxInput("showTrend", "Show Trend Line", value = TRUE),
      hr(),
      actionButton("updateSummary", "Update Summary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Line Plot", plotOutput("linePlot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive data filtered by selected month
  filteredData <- reactive({
    airquality %>%
      filter(Month == as.numeric(input$month)) %>%
      drop_na(input$yvar)
  })
  
  # Line Plot
  output$linePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Day, y = get(input$yvar))) +
      geom_line(color = "blue", size = 1) +
      theme_minimal() +
      labs(title = paste("Daily", input$yvar, "in Selected Month"),
           x = "Day of the Month",
           y = input$yvar) +
      if (input$showTrend) geom_smooth(method = "lm", color = "red", se = FALSE)
  })
  
  # Summary Statistics
  observeEvent(input$updateSummary, {
    output$summary <- renderPrint({
      summary(filteredData()[[input$yvar]])
    })
  })
}

# Run the Application
shinyApp(ui = ui, server = server)

