library(shiny)
library(quantmod)  
library(ggplot2)   


ui <- fluidPage(
  titlePanel("Real-Time Stock Price Monitor"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Enter Stock Symbol:", value = "AAPL"),  
      dateRangeInput("dateRange", "Date Range:",
                     start = Sys.Date() - 30, end = Sys.Date()),  
      actionButton("getData", "Get Data")  
    ),
    
    mainPanel(
      plotOutput("stockPlot"),  
      textOutput("alert")       
    )
  )
)


server <- function(input, output) {
  # Fetch stock data when button is clicked
  stock_data <- eventReactive(input$getData, {
    getSymbols(input$symbol, src = "yahoo", from = input$dateRange[1], 
               to = input$dateRange[2], auto.assign = FALSE)
  })
  
  # Render the stock price plot
  output$stockPlot <- renderPlot({
    chartSeries(stock_data(), theme = chartTheme("white"), 
                name = paste("Stock Prices:", input$symbol))
  })
  
  
  output$alert <- renderText({
    last_price <- as.numeric(Cl(stock_data())[nrow(stock_data()), ])
    if (last_price > 150) {
      "Price is high, consider selling."
    } else {
      "Price is low, consider buying."
    }
  })
}


shinyApp(ui = ui, server = server)

