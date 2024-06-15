
# >> Movie Rating Dashboard
# Purpose: This application visualizes movie ratings and allows users to filter data based on various criteria such as genre, year, and rating.
# 
# Features:
#   
# 1. Interactive filters for genre, year, and rating
# 2. Dynamic plots showing the distribution of ratings
# 3. Data table to display filtered results



# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Sample movie ratings data
movies <- data.frame(
  title = c("Movie A", "Movie B", "Movie C", "Movie D", "Movie E"),
  genre = c("Action", "Comedy", "Drama", "Action", "Comedy"),
  year = c(2010, 2012, 2015, 2020, 2018),
  rating = c(7.8, 6.5, 8.2, 7.0, 6.9)
)

# UI
ui <- fluidPage(
  titlePanel("Movie Rating Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("genre", "Select Genre:", choices = unique(movies$genre), selected = "All", multiple = TRUE),
      sliderInput("year", "Select Year:", min = 2010, max = 2020, value = c(2010, 2020)),
      sliderInput("rating", "Select Rating:", min = 0, max = 10, value = c(0, 10))
    ),
    mainPanel(
      plotOutput("ratingPlot"),
      DTOutput("moviesTable")
    )
  )
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    data <- movies
    if (!is.null(input$genre)) {
      data <- data %>% filter(genre %in% input$genre)
    }
    data <- data %>% filter(year >= input$year[1], year <= input$year[2])
    data <- data %>% filter(rating >= input$rating[1], rating <= input$rating[2])
    data
  })
  
  output$ratingPlot <- renderPlot({
    ggplot(filteredData(), aes(x = rating)) +
      geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = "Distribution of Movie Ratings", x = "Rating", y = "Count")
  })
  
  output$moviesTable <- renderDT({
    datatable(filteredData())
  })
}

# Run the application
shinyApp(ui = ui, server = server)

