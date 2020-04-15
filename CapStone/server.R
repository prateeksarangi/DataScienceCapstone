library(shiny)

source("./wordPredictor.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    prediction <- reactive({predict_next_word(input$InputSentence)})
    output$predictedWord <- renderText(prediction())
    
})