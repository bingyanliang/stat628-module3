library(shiny)

reviews = read.csv('bot_sentiment.csv')

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("1-Star Reviews Viewer"),
  
  HTML("<p>Most reviews have a positive sentiment score, 
  regardless of star ratings. Therefore, we think it would 
  be more helpful to check negative reviews and know what 
  people feel most strongly about.</p>
  <p>You can input some text (case-incensitive) and check
       relative reviews. Relative reviews are randomly selected;
       to begin with, the 4 most mentioned
       bigrams are 'taco bell', 'customer service', 'sour cream',
        'taco bus'.</p>"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      textInput(inputId = "text",
                  label = "Check a text:",
                value = 'customer service'),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view in random:",
                   value = 5)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  datasetInput <- reactive({
    reviews[grep(tolower(input$text), tolower(reviews$text)), ]
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # Show 'n' random observations of the filtered dataset ----
  output$view <- renderTable({
    sampled_data <- datasetInput()
    if (nrow(sampled_data) > input$obs) {
      sampled_data <- sampled_data[sample(nrow(sampled_data), size = input$obs), ]
    }
    sampled_data
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)