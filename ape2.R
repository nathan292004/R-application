library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Explore the mtcars Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X-axis Variable:", choices = names(mtcars)),
      selectInput("yvar", "Y-axis Variable:", choices = names(mtcars)),
      checkboxInput("showTrend", "Show Trend Line", value = FALSE),
      hr(),
      selectInput("summaryVar", "Variable to Summarize:", choices = names(mtcars)),
      actionButton("updateSummary", "Update Summary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Scatter Plot
  output$scatterPlot <- renderPlot({
    ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "blue", size = 3) +
      theme_minimal() +
      labs(x = input$xvar, y = input$yvar, title = "Scatter Plot") +
      if (input$showTrend) geom_smooth(method = "lm", color = "red", se = FALSE)
  })
  
  # Summary Statistics
  observeEvent(input$updateSummary, {
    output$summary <- renderPrint({
      summary(mtcars[[input$summaryVar]])
    })
  })
}

# Run the Application
shinyApp(ui = ui, server = server)

