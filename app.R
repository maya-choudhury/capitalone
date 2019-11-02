library(shiny)
library(shinythemes)
library(readr)

# Load data
jeopardy <- read_csv("data.csv", col_types = cols(game_id = col_double()))
library(tidyverse)

names(jeopardy)[names(jeopardy) == "title"] <- "category"

ui <- fluidPage(theme = shinytheme("yeti"),
                titlePanel("Maya's Jeopardy Web App"),
                sidebarLayout(
                  sidebarPanel(
                    
                    #trivia category
                    #level of difficulty of the question
                    selectInput(inputId = "dif", label = strong("Level of Difficulty"),
                                choices = c("Any" = "Any", "100" = "100",  "200" = "200",  "300" = "300",  "400"="400", 
                                            "500"="500", "800"="800", "600"="600", "1000"="1000"),
                                selected = "Any"),
                    
                    # Select date range to be plotted, date or timeframe aired (you can search by a day,  a week, a month)
                    dateRangeInput("date", label =  strong("Date(s) of Episodes"), start = "1984-09-10", end = "2015-03-31",
                                   min = "1984-09-10", max = "2015-03-31"),
                    
                    
                    textInput("category", label = strong("Jeopardy Category")),
                    helpText("Note: There are set categories that Jeopardy
                             uses. To see how the category search feature works, 
                             try 'shells' or 'rhyme time'"),
                    checkboxInput(inputId = "random", label = "Select Random Game", value=FALSE), 
                    checkboxInput(inputId = "final", label = "Simulate Final Jeopardy", value=FALSE)                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    textOutput("randomtext"), 
                    DT::dataTableOutput("out"),
                    textOutput("finaltext"), 
                    dataTableOutput("random"),
                    dataTableOutput("final"),
                    textOutput("randfinal"), 
                    tags$a(href = "http://www.jservice.io/", "Source: Jeopardy API")
                  )
                )
)

server <- function(input, output, session) {
  
  searchreact <- reactive({
    if(!is.null(input$dif)){
      if(input$dif!="Any"){
        jeopardy <- jeopardy %>%
          filter(value == as.integer(input$dif)) 
      }
    }
    if(!is.null(input$date)){
      jeopardy <- jeopardy %>%
        filter(airdate <= input$date[2] & airdate>=input$date[1]) 
    }
    if(input$category!=""){
      jeopardy <- jeopardy %>%
        filter(category == input$category)    }
    
    jeopardy %>% 
      select(question,value, airdate, category, answer) 
  })
  
  finalreact <- reactive({
    if(!is.null(input$final)  & input$random==FALSE){
      finalq <- jeopardy %>% 
        filter(value==1000) %>% 
        sample_n(1) %>% 
        select(question, category, answer)
    }
    finalq
  })
  
  randomreact <- reactive({ # this function seems to work
    if(!is.null(input$random) & input$final==FALSE){
      
      random_jeopardy <- jeopardy %>%  
        filter(!is.na(question), !is.na(answer), airdate==sample(unique(airdate),1)) %>% 
        select(question,value, airdate, category, answer) %>% 
        arrange(category)
    }
    random_jeopardy 
    
  }) 
  
  output$randomtext <- renderText({
    if(input$random & input$final==FALSE){
    "We generated all of the questions from one Jeopardy episode for you 
        to play!"
    }
  })
  
  futuretime <- eventReactive(input$final, { 
    time <- as.POSIXlt(Sys.time()+30)
    time
    })
  
  output$finaltext <- renderText({
    if(input$final & input$random==FALSE){ 
      invalidateLater(1, session)
      x <- futuretime()
      time <- strptime((x-Sys.time()), "%S")
      paste("We chose a random 1000 level question for you to test out your 
      skills! You have ", time, " seconds left. Contestants usually get 30 seconds
      for Final Jeopardy. Once your time is up, we recommend searching within 
      the 1000 value for the question's category to find the right answer!")
    
      }
  })
  output$out <- DT::renderDataTable({
    if(!input$final & !input$random){
      jeopardy <- searchreact()
      n <- nrow(jeopardy)
      if(n>100){
        n <-100
      }
      set_jeopardy <- jeopardy %>%
        filter(!is.na(question), !is.na(answer)) %>%
        sample_n(n) %>%
        select(question,value, airdate, category, answer)
      
      set_jeopardy
    }
  })
  
  output$randfinal <- renderText({ 
    if(input$random & input$final){
      'Please select either the random game OR final jeopardy option!'
  }})
  
  output$random <- renderDataTable(
    if(input$random  & input$final==FALSE){
      random <- randomreact()
      random
    }
  )
  
  finalanswer <- function(q) q
  output$final <- renderDataTable(
    if(input$final & !input$random){
      finalq <- finalreact()
      final <- finalq %>% 
        select(question, category)
      final
    }
  )
  
}

shinyApp(ui = ui, server = server)