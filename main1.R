# Install necessary libraries if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("plotly")) install.packages("plotly")
if (!require("leaflet")) install.packages("leaflet")

# Example dataset for demonstration
yt_data <- data.frame(
  Channel = c("PewDiePie", "T-Series", "Cocomelon", "SET India", "Music", 
              "Kids Diana Show", "Like Nastya", "Vlad and Niki", "5-Minute Crafts", "Zee TV"),
  WatchHours = sample(1000000:5000000, 10),  # Simulating watch hours
  AudienceCountry = c("Sweden", "India", "USA", "India", "Global", "USA", 
                      "Russia", "USA", "Global", "India"),
  Lat = c(59.3293, 20.5937, 37.0902, 20.5937, 0, 37.0902, 55.7558, 37.0902, 38.9637, 28.6139),  # Example latitudes
  Lon = c(18.0686, 78.9629, -95.7129, 78.9629, 0, -95.7129, 37.6173, -95.7129, 35.2433, 77.2090)   # Example longitudes
)

# Shiny UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "YouTube Channel Analysis Dashboard", titleWidth = 400),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Top Channels", tabName = "top_channels", icon = icon("youtube")),
      menuItem("Audience Distribution", tabName = "audience", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(
        "body { background-color: #ffffff; color: #000000; }
         .box { background: #f9f9f9; color: #000000; border-color: #cccccc; }
         .content-wrapper { background: #ffffff; }
         .sidebar { background: #f4f4f4; }
         .main-header .logo { background: #ffffff; color: #000000; }
         .main-header .navbar { background: #ffffff; }
         .main-sidebar .sidebar-menu .active a { background: #cccccc; color: #000000; }
         .main-sidebar .sidebar-menu a { color: #333333; }
         .box-header { background: #eaeaea; color: #000000; }
        "
      ))
    ),
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Top 10 Channels by Watch Hours", width = 12, status = "primary",
                    plotlyOutput("topChannelsBar"))
              )
      ),
      
      # Top channels tab
      tabItem(tabName = "top_channels",
              fluidRow(
                box(title = "Channel Statistics", width = 12, status = "info",
                    verbatimTextOutput("channelStats"))
              )
      ),
      
      # Audience distribution tab
      tabItem(tabName = "audience",
              fluidRow(
                box(title = "Audience Locations", width = 12, status = "success",
                    leafletOutput("audienceMap", height = 600))
              ),
              fluidRow(
                box(title = "Audience Distribution by Country", width = 12, status = "warning",
                    plotlyOutput("audiencePie"))
              )
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Top channels bar chart
  output$topChannelsBar <- renderPlotly({
    p <- ggplot(yt_data, aes(x = reorder(Channel, -WatchHours), y = WatchHours, fill = Channel)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Top 10 Channels by Watch Hours", x = "Channel", y = "Watch Hours") +
      theme_minimal(base_family = "Arial", base_size = 14) +
      theme(
        plot.background = element_rect(fill = "#ffffff", color = NA),
        panel.background = element_rect(fill = "#f9f9f9", color = NA),
        axis.text = element_text(color = "#000000"),
        axis.title = element_text(color = "#000000")
      )
    ggplotly(p) %>% layout(paper_bgcolor = '#ffffff', plot_bgcolor = '#ffffff')
  })
  
  # Channel statistics
  output$channelStats <- renderPrint({
    yt_data %>% arrange(desc(WatchHours))
  })
  
  # Audience map
  output$audienceMap <- renderLeaflet({
    leaflet(yt_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon, lat = ~Lat,
        radius = ~sqrt(WatchHours / 100000),  # Adjust size based on watch hours
        color = "blue", fill = TRUE, fillOpacity = 0.6,
        popup = ~paste(
          "<strong>", Channel, "</strong><br/>",
          "Watch Hours: ", WatchHours, "<br/>",
          "Audience Country: ", AudienceCountry
        )
      )
  })
  
  # Audience distribution pie chart
  output$audiencePie <- renderPlotly({
    audienceData <- yt_data %>% count(AudienceCountry) %>%
      mutate(Percentage = n / sum(n) * 100)
    
    plot_ly(audienceData, labels = ~AudienceCountry, values = ~Percentage, type = 'pie',
            textinfo = 'label+percent',
            title = "Audience Distribution by Country",
            marker = list(colors = c("#ff9999", "#66b3ff", "#99ff99", "#ffcc99", "#c2c2f0", "#ff6666")),
            paper_bgcolor = '#ffffff',
            textfont = list(color = '#000000'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)