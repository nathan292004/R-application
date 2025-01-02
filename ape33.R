library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Explore the ChickWeight Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("diet", "Choose Diet Group:", 
                  choices = unique(ChickWeight$Diet), 
                  selected = unique(ChickWeight$Diet)[1]),
      sliderInput("timeRange", "Select Time Range:", 
                  min = min(ChickWeight$Time), 
                  max = max(ChickWeight$Time), 
                  value = range(ChickWeight$Time)),
      hr(),
      actionButton("update", "Update Plot")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Weight Over Time", plotOutput("timePlot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive data filtered by user inputs
  filteredData <- reactive({
    ChickWeight %>%
      filter(Diet == input$diet, 
             Time >= input$timeRange[1], 
             Time <= input$timeRange[2])
  })
  
  # Plot: Weight Over Time
  output$timePlot <- renderPlot({
    ggplot(filteredData(), aes(x = Time, y = weight, group = Chick)) +
      geom_line(color = "blue") +
      theme_minimal() +
      labs(title = "Chick Weight Over Time", 
           x = "Time (days)", 
           y = "Weight (grams)")
  })
  
  # Summary Statistics
  observeEvent(input$update, {
    output$summary <- renderPrint({
      filteredData() %>%
        summarise(
          MinWeight = min(weight),
          MaxWeight = max(weight),
          MeanWeight = mean(weight),
          MedianWeight = median(weight)
        )
    })
  })
}

# Run the Application
shinyApp(ui = ui, server = server)
