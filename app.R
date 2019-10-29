library(shiny)
library(shinythemes)
library(readr)

# Load data
jeopardy <- read_csv("data.csv", col_types = cols(game_id = col_double()))

jeopardy

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Jeopardy"),
                sidebarLayout(
                  sidebarPanel(
                    
                    #trivia category
                    #level of difficulty of the question
                    selectInput(inputId = "dif", label = strong("Level of Difficulty"),
                                choices = c("100" = "100",  "200" = "200",  "300" = "300",  "400"="400", 
                                            "500"="500", "800"="800", "600"="600", "1000"="1000"),
                                selected = "100"),
                    
                    # Select date range to be plotted, date or timeframe aired (you can search by a day,  a week, a month)
                    dateRangeInput("date", strong("Date range"), start = "1985-02-08", end = "2019-07-31",
                                   min = "1985-02-08", max = "2019-07-31"),
                    
                    textInput("category", strong("Jeopardy category"))
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot"),
                    tags$a(href = "http://www.jservice.io/", "Source: Jeopardy API")
                  )
                )
)

server <- function(input, output) {
  # Subset data
  set_jeopardy <- reactive({
    req(input$date)
    validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
    jeopardy %>%
      filter(
        type == input$type,
        airdate > as.POSIXct(input$date[1]) & airdate < as.POSIXct(input$date[2], 
                                                                   value == input$dif
                                                                   # need to filter for category somehow
        ))
  })
  
  output$lineplot <- renderPlot({
    plot(jeopardy$value, jeopardy$category_id)
  })
  
}
shinyApp(ui = ui, server = server)