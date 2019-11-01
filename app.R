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
                    #textOutput("final"),
                    tags$a(href = "http://www.jservice.io/", "Source: Jeopardy API")
                  )
                )
)

server <- function(input, output) {
  # Subset data
  
  getInput <- reactive({
    getSymbols(
    difficulty = input$dif,
    type = input$type,
    date1 = input$date[1],
    date2 = input$date[2],
    rand = input$random,
    final = input$final,
    auto.assign = FALSE)
  })

  # set_jeopardy <- reactive({
  #   if (difficulty>0) {
  #     jeopardy %>%
  #       filter(value == difficulty)
  #   }
  # })
  #   if (date1>'1984-09-10' || date2 < '2015-03-31'){
  #     jeopardy %>%
  #       filter(airdate <= date2 & airdate>=date1)
  #   }
  #   if (type!=""){
  #     jeopardy %>%
  #       filter(title == type)
  #   }
  #   jeopardy %>%
  #     slice(1:25)
  # })
  # 
  # finaljeop <- reactive({
  #   if (final==TRUE) {
  #     jeopardy %>%
  #       filter(value==1000) %>%
  #       sample_n(1)
  #   }
  # })
  # 
  # output$final <- renderText({
  #   'There are 30 seconds alloted for final jeopardy' # + finaljeop['question']
  #   # reactiveTimer(intervalMs = 1000)
  # })
  # 
  output$out <- DT::renderDataTable({
    difreact <- eventReactive( input$dif, {
      jeopardy %>%
        filter(value == input$dif)
      })
    datereact <- eventReactive(input$date, {
      jeopardy %>%
        filter(airdate <= input$date[2] & airdate>=input$date[1])
    })
    typereact <- eventReactive(input$type, {
      jeopardy %>%
        filter(title == input$type)
    })
  set_jeopardy <- jeopardy %>%
    filter(!is.na(question), !is.na(answer)) %>% 
    sample_n(25) %>% 
    select(question, answer)
  
  set_jeopardy
  })
  
}

shinyApp(ui = ui, server = server)