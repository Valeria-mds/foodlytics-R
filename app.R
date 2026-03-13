library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Load data 
data_path <- "data/raw/cleaned_full_data.csv"
df <- read.csv("data/cleaned_full_data.csv")

# Fix typo from original dataset
df$city <- gsub("Branpton", "Brampton", df$city, fixed = TRUE)

# Choices for input
CITIES <- sort(unique(df$city))

# UI 
ui <- fluidPage(
  
  titlePanel("Foodlytics Dashboard (R Shiny)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId = "city",
        label = "Select City",
        choices = CITIES,
        selected = CITIES[1]
      )
      
    ),
    
    mainPanel(
      
      plotOutput("plot_cuisine"),
      
      br(),
      
      h4("Restaurants"),
      DTOutput("restaurant_table")
      
    )
    
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive calculation
  filtered_df <- reactive({
    
    df |>
      filter(city == input$city)
    
  })
  
  # Output 1: Bar chart
  output$plot_cuisine <- renderPlot({
    
    data <- filtered_df()
    
    if (nrow(data) == 0) {
      plot.new()
      text(0.5, 0.5, "No restaurants in this city")
      return()
    }
    
    data |>
      count(category_1, name = "count") |>
      arrange(desc(count)) |>
      slice_head(n = 15) |>
      ggplot(aes(x = reorder(category_1, count), y = count)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        title = paste("Restaurant Count by Cuisine in", input$city),
        x = "Cuisine",
        y = "Number of Restaurants"
      ) +
      theme_minimal()
    
  })
  
  # Output 2: Table
  output$restaurant_table <- renderDT({
    
    filtered_df() |>
      select(
        restaurant,
        star,
        num_reviews,
        price_range,
        category_1
      ) |>
      rename(
        Restaurant = restaurant,
        Rating = star,
        Reviews = num_reviews,
        Price = price_range,
        Cuisine = category_1
      )
    
  })
}

# Run app
shinyApp(ui = ui, server = server)