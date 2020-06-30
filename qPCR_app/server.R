library(shiny)
library(shinyFiles)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    results <- observe({
        req(input$files)
        inFiles <- input$files
        #results <- read_csv(inFile$datapath)
        
        output$rawData <- DT::renderDataTable({})
        
        

    })

})
