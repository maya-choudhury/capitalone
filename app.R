library(shiny)
library(shinythemes)
library(readr)

# Load data
jeopardy <- read_csv("data.csv", col_types = cols(game_id = col_double()))
library(tidyverse)

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
                    dateRangeInput("date", label =  strong("Date(s) of Episodes"), start = "1984-09-10", end = "2015-03-31",
                                   min = "1984-09-10", max = "2015-03-31"),
                    
                    textInput("category", label = strong("Jeopardy Category")), 
                    helpText("Note: There are set categories that Jeopardy
                             uses. To see one try 'comedians' or 'hollywood legends'"),
                    checkboxInput(inputId = "random", label = "Select Random Game", value=FALSE), 
                    checkboxInput(inputId = "final", label = "Simulate Final Jeopardy", value=FALSE)
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    DT::dataTableOutput("out"),
                    dataTableOutput("final"),
                    tags$a(href = "http://www.jservice.io/", "Source: Jeopardy API")
                  )
                )
)

server <- function(input, output) {
    
  searchreact <- reactive({
    if(!is.null(input$dif)){
      jeopardy <- jeopardy %>%
        filter(value == as.integer(input$dif))
    }
    if(!is.null(input$date)){
      jeopardy <- jeopardy %>%
        filter(airdate <= input$date[2] & airdate>=input$date[1]) 
    }
    if(!is.null(input$type)){
        jeopardy <- jeopardy %>%
          filter(title == input$type)    }
    
    jeopardy
  })
  
  finalreact <- reactive({
    if(!is.null(input$final)){
      finalq <- jeopardy %>% 
        filter(value==1000) %>% 
        sample_n(1) %>% 
        select(question, answer)
    }
    finalq
  })
 
  randomreact <- reactive({
    if(!is.null(input$random)){
      random_jeopardy <- jeopardy %>%  
        sample_n(25)  %>%
        select(question,value, airdate, answer)
    }
    'There are 30 seconds alloted for final jeopardy'
    # reactiveTimer(intervalMs = 1000)
    random_jeopardy 
  }) 
  
  
  output$out <- DT::renderDataTable({
    if(!input$final & !input$random){
      jeopardy <- searchreact()
      set_jeopardy <- jeopardy %>%
        filter(!is.na(question), !is.na(answer)) %>%
        sample_n(25) %>%
        select(question,value, airdate, answer) # add category

      set_jeopardy
    }
    
    if(!input$final){
      random <- randomreact()
      random
    }
    
  })
  
  output$text <- renderDataTable(
    finalq <- finalreact()
    finalq
  )
  
}

shinyApp(ui = ui, server = server)