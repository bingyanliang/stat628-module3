
library(shiny)
library(shinythemes)  
library(ggplot2)
library(leaflet)
library(dplyr)
library(tidytext)
library(syuzhet)
library(wordcloud)
library(cowplot) 


da2 <- read.csv('Tampa_mexican_restaurants.csv')
trips <- read.csv('hillsborough.csv')
reviews <- read.csv('bot_sentiment.csv')


ui <- fluidPage(
  theme = shinytheme("flatly"),  
  titlePanel("Yelp Data Analysis - Mexican Restaurants in Hillsborough, FL", windowTitle = "Welcome to Use Our Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedPage", "Choose a Page:", choices = c("Main Page", "Restaurant Analysis", "Travel Analysis", "1-Star Reviews Viewer"))
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.selectedPage === 'Main Page'",
        div(class = "welcome-message",
            h1("Welcome to Use Our Dashboard", class = "main-title"),
            h2("Overview", class = "overview-title"),
            p("Our recommendations are based on data analysis from Yelp, U.S. Bureau of Transportation Statistics, and US Census Bureau.",
              class = "overview-text"),
            hr(),
            div(class = "recommendations",
                h3("Overall Recommendations", class = "rec-title"),
                p("Geographical Recommendations: Zip codes 33603, 33604, 33614 in Tampa City, or 33570 in Sun City.", class = "rec-text"),
                p("Restaurant Attributes: No TV, cautious with delivery service.", class = "rec-text"),
                p("Quality Control: considerations for sour cream.", class = "rec-text"),
                p("Improvements: ordering process, staff training, especially for managers.", class = "rec-text"),
                p("Topics related to Chipotle: Overall improvements are needed.", class = "rec-text")
            ),
            hr(),
            h3("Restaurant Analysis", class = "analysis-title"),
            p("This section displays specific information about the restaurant, as well as potential areas for improvement derived from sentiment analysis.", class = "analysis-text"),
            h3("Travel Analysis", class = "analysis-title"),
            p("This part is the comparison of the number of travelers before and after the Covid-19 obtained by analyzing the U.S. Bureau of Transportation Statistics data. We can conclude that compared with the epidemic period, the number of travelers has returned to the pre-epidemic level. There is no need to worry about passenger flow!", class = "analysis-text"),
            hr(),
            h3("Contact Us", class = "contact-title"),
            p(strong("Feel free to reach out to any of our group members:"),
              br(),
              "Ruofeng Tang: rtang64@wisc.edu",
              br(),
              "Ziming Li: zli2543@wisc.edu",
              br(),
              "Bingyan Liang: bliang34@wisc.edu",
              class = "contact-text")
        ),
        style = "padding: 20px;"
      ),
      
      conditionalPanel(
        condition = "input.selectedPage === 'Restaurant Analysis'",
        selectInput("selectedCity", "Select a City:", choices = unique(da2$city)),
        textInput("restaurantName", "Enter Restaurant Name:", value = ""),
        leafletOutput("map"),
        plotOutput("starHistogram"),
        verbatimTextOutput("sentimentAnalysis"),
        plotOutput("wordCloud")
      ),
      
      conditionalPanel(
        condition = "input.selectedPage === 'Travel Analysis'",
        selectInput("selectedComparison", "Select Comparison Type:", 
                    choices = c("Percentage by Month", "Percentage by Day", "Trips by Month", "Trips by Day",
                                "Pre-Covid Comparison", "During-Covid Comparison", "After-Covid Comparison")),
        plotOutput("comparisonPlot")
      ),
      
      conditionalPanel(
        condition = "input.selectedPage === '1-Star Reviews Viewer'",
        HTML("<p>Most reviews have a positive sentiment score, regardless of star ratings. Therefore, we think it would be more helpful to check negative reviews and know what people feel most strongly about.</p>
              <p>You can input some text (case-insensitive) and check relative reviews. Relative reviews are randomly selected; to begin with, the 4 most mentioned bigrams are 'taco bell', 'customer service', 'sour cream', 'taco bus'.</p>"),
        sidebarPanel(
          textInput(inputId = "text", label = "Check a text:", value = 'customer service'),
          numericInput(inputId = "obs", label = "Number of observations to view in random:", value = 5)
        ),
        mainPanel(
          verbatimTextOutput("summary"),
          tableOutput("view")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filteredData <- reactive({
    req(input$selectedPage == "Restaurant Analysis")
    da2 %>% filter(city == input$selectedCity)
  })
  
  output$map <- renderLeaflet({
    df <- filteredData()
    if(nrow(df) > 0) {
      df_aggregated <- df %>%
        group_by(business_id, name, address, latitude, longitude) %>%
        summarise(average_stars = mean(stars_y, na.rm = TRUE), 
                  total_reviews = sum(review_count, na.rm = TRUE), .groups = 'drop')
      
      m <- leaflet(df_aggregated) %>%
        addTiles() %>%
        setView(lng = mean(df_aggregated$longitude), lat = mean(df_aggregated$latitude), zoom = 9) %>%
        addMarkers(~longitude, ~latitude, 
                   popup = ~paste(name, "<br>", address, "<br>Average Stars:", average_stars, "<br>Total Reviews:", total_reviews),
                   clusterOptions = markerClusterOptions())
      m
    }
  })
  
  output$starHistogram <- renderPlot({
    df <- filteredData()
    if(nrow(df) > 0) {
      ggplot(df, aes(x = stars_y)) +
        geom_histogram(binwidth = 0.5, fill = "blue", color = "white") +
        theme_minimal() +
        labs(title = paste("Star Distribution in", input$selectedCity), x = "Stars", y = "Count")
    }
  })
  
  output$sentimentAnalysis <- renderText({
    req(input$restaurantName, input$selectedPage == "Restaurant Analysis")
    df <- filteredData()
    restaurant_reviews <- df %>% filter(name == input$restaurantName) %>% select(text)
    
    if(nrow(restaurant_reviews) > 0) {
      sentiment <- get_sentiments("bing")
      words <- restaurant_reviews %>% 
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        inner_join(sentiment, by = "word") %>%
        filter(sentiment == "negative") %>%
        summarise(negative_words_count = n(), .groups = 'drop')
      
      if(is.null(words) || nrow(words) == 0 || words$negative_words_count == 0) {
        "No negative words found for this restaurant."
      } else {
        paste("Number of negative words for", input$restaurantName, ":", words$negative_words_count)
      }
    } else {
      "No reviews found for this restaurant."
    }
  })
  
  output$wordCloud <- renderPlot({
    req(input$restaurantName, input$selectedPage == "Restaurant Analysis")
    df <- filteredData()
    restaurant_reviews <- df %>% filter(name == input$restaurantName) %>% select(text)
    
    if(nrow(restaurant_reviews) > 0) {
      words <- restaurant_reviews %>% 
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE) %>%
        filter(n > 1)
      
      wordcloud(words$word, words$n, max.words = 100)
    }
  }, width = 800, height = 600)
  
  output$comparisonPlot <- renderPlot({
    req(input$selectedPage == "Travel Analysis")
    switch(input$selectedComparison,
           "Percentage by Month" = plot_grid(p1a, p1b, p1c, nrow = 1, ncol = 3),
           "Percentage by Day" = plot_grid(p2a, p2b, p2c, nrow = 1, ncol = 3),
           "Trips by Month" = plot_grid(p3a, p3b, p3c, nrow = 1, ncol = 3),
           "Trips by Day" = plot_grid(p4a, p4b, p4c, nrow = 1, ncol = 3),
           "Pre-Covid Comparison" = plot_grid(p1a, p3a, p2a, p4a, nrow = 2, ncol = 2),
           "During-Covid Comparison" = plot_grid(p1b, p3b, p2b, p4b, nrow = 2, ncol = 2),
           "After-Covid Comparison" = plot_grid(p1c, p3c, p2c, p4c, nrow = 2, ncol = 2)
    )
  })
  
  datasetInput <- reactive({
    reviews[grep(tolower(input$text), tolower(reviews$text)), ]
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    sampled_data <- datasetInput()
    if (nrow(sampled_data) > input$obs) {
      sampled_data <- sampled_data[sample(nrow(sampled_data), size = input$obs), ]
    }
    sampled_data
  })
}

shinyApp(ui = ui, server = server)
