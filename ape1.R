# Install required packages if not already installed
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}
if (!requireNamespace("nnet", quietly = TRUE)) {
  install.packages("nnet")
}
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}

# Load libraries
library(shiny)
library(nnet)
library(plotly)

# Load the Iris dataset
data(iris)

# Train a neural network model
set.seed(123)  # For reproducibility
iris_nn <- nnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                data = iris, size = 5, linout = FALSE, maxit = 1000)

# Define UI
ui <- fluidPage(
  titlePanel("Iris Species Prediction with Neural Networks"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("sepal_length", "Sepal Length (cm):", value = 5.0, min = 4.0, max = 8.0, step = 0.1),
      numericInput("sepal_width", "Sepal Width (cm):", value = 3.0, min = 2.0, max = 4.5, step = 0.1),
      numericInput("petal_length", "Petal Length (cm):", value = 3.5, min = 1.0, max = 7.0, step = 0.1),
      numericInput("petal_width", "Petal Width (cm):", value = 1.0, min = 0.1, max = 2.5, step = 0.1),
      actionButton("predict", "Predict Species")
    ),
    
    mainPanel(
      h3("Prediction Result:"),
      verbatimTextOutput("prediction"),
      h4("Interactive Scatter Plot: Sepal Length vs Sepal Width"),
      plotlyOutput("scatter_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to make predictions using the trained neural network
  predicted_species <- reactive({
    req(input$predict)  # Ensure the button is clicked
    
    # Prepare input data
    input_data <- data.frame(
      Sepal.Length = input$sepal_length,
      Sepal.Width = input$sepal_width,
      Petal.Length = input$petal_length,
      Petal.Width = input$petal_width
    )
    
    # Make the prediction
    prediction <- predict(iris_nn, input_data, type = "class")
    
    return(prediction)
  })
  
  # Render the prediction result
  output$prediction <- renderPrint({
    predicted_species()
  })
  
  # Render the interactive scatter plot using plotly
  output$scatter_plot <- renderPlotly({
    p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point() +
      labs(title = "Sepal Length vs Sepal Width by Species",
           x = "Sepal Length (cm)", y = "Sepal Width (cm)") +
      theme_minimal()
    
    ggplotly(p)  # Convert the ggplot object to an interactive plotly chart
  })
}

# Run the application
shinyApp(ui = ui, server = server)
