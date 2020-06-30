library(shiny)
library(shinyFiles)
library(DT)

shinyUI(fluidPage(

    # Application title
    titlePanel("I love the smell of petunia in the morning"),

    # Sidebar with a slider input for number of bins
    sidebarPanel(
        
        # Input: Select a file ----
        fileInput("files", "Upload", multiple = TRUE, accept = c(".xls"))
),
    mainPanel(DT::dataTableOutput("rawData"))
))
