# Movie Revenue Predictor
# Purpose: This application uses a machine learning model to predict movie revenue based on user inputs such as budget, genre, director popularity, and release month.
# 
# Features:
#   
# 1.  Input fields for budget, genre, director popularity, and release month
# 2.  Machine learning model to predict revenue
# 3.  Display predicted revenue



# Load necessary libraries
library(shiny)
library(caret)
library(randomForest)

# Sample model training data (for demonstration purposes)
set.seed(123)
train_data <- data.frame(
  budget = runif(100, 1e6, 1e8),
  genre = factor(sample(c("Action", "Comedy", "Drama"), 100, replace = TRUE)),
  director_popularity = runif(100, 1, 10),
  release_month = factor(sample(1:12, 100, replace = TRUE)),
  revenue = runif(100, 1e6, 1e9)
)

# Train a simple random forest model
model <- randomForest(revenue ~ budget + genre + director_popularity + release_month, data = train_data)

# UI
ui <- fluidPage(
  titlePanel("Movie Revenue Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput("budget", "Budget (in millions):", value = 50, min = 1, max = 100),
      selectInput("genre", "Genre:", choices = c("Action", "Comedy", "Drama")),
      sliderInput("director_popularity", "Director Popularity (1-10):", min = 1, max = 10, value = 5),
      selectInput("release_month", "Release Month:", choices = 1:12, selected = 6)
    ),
    mainPanel(
      textOutput("predictedRevenue")
    )
  )
)

# Server
server <- function(input, output) {
  predictRevenue <- reactive({
    new_data <- data.frame(
      budget = input$budget * 1e6,
      genre = factor(input$genre, levels = levels(train_data$genre)),
      director_popularity = input$director_popularity,
      release_month = factor(input$release_month, levels = levels(train_data$release_month))
    )
    predict(model, new_data)
  })
  
  output$predictedRevenue <- renderText({
    revenue <- predictRevenue()
    paste("Predicted Revenue: $", round(revenue, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
