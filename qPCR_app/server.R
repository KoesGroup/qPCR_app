library(shiny)
library(shinyFiles)
library(tidyverse)
library(data.table)

source("FetchData.R")
source("infoFile.R")

server <- function(input, output) {
  output$intro1 <- renderText("info box. Upon clicking on an info button, information will be displayed here.")
  rv <- reactiveValues(df = NULL, rawDf = NULL, targets = NULL, genes = NULL, refs = NULL, normDf = NULL,
                       samps = NULL, sam=NULL, sampleText = NULL, data2plot=NULL)  
  event1 <- observeEvent(input$goButton, {   #submit input counts.txt
    output$intro1 <- renderText("Info box")
    req(input$csvs)
    rv$df <- input$csvs$datapath
    total <- data.frame()
    #result <- unlist(strsplit(dataDir, '\\t+')[[1]])
    for(f in 1:length(rv$df)){
        print(f)
        PCRdata <- read_excel(input$csvs[[f, 'datapath']], sheet = "Results", col_names = TRUE, col_types = NULL, na = "", skip =43 )
        PCRsubset <- PCRdata %>%
          select("Sample Name","Target Name","CT") %>%
          as.data.frame()
        colnames(PCRsubset) <- c("Sample","Target","CT")
        PCRsubset <- PCRsubset[complete.cases(PCRsubset),]
        PCRsubset$CT <- as.double(PCRsubset$CT)
        if(nrow(total) == 0){
          total <- PCRsubset
        } else {
          total <- merge(total,PCRsubset,all = T)
        }
      rv$rawDf <- total

    }

  output$contents <- renderTable({   
    rv$rawDf
      }, rownames = T)
  
  geneList <- TargetValues(rv$rawDf)
  rv$genes <- geneList$Target
  })
  
  event2 <- observeEvent(input$outButton, {
    output$intro1 <- renderText("Info box")
    MaxDif <- input$OutTrash
    output$contents <- renderTable({   
      GetOutlayers(rv$rawDf, maxDif=MaxDif)
    }, rownames = T)
  }) 
 
  event3 <- observeEvent(input$TargetButton, {
    output$intro1 <- renderText("Info box")
    geneList <- TargetValues(rv$rawDf)
    rv$genes <- geneList$Target
    output$contents <- renderTable({   
      geneList
    }, rownames = F)
  })
  
 
  output$border1 <- renderText("<hr style=\"height:3px;border-width:0;color:#9E9E9C;background-color:#9E9E9C\">")
  
  output$border2 <- renderText("<hr style=\"height:3px;border-width:0;color:#9E9E9C;background-color:#9E9E9C\"><strong>List of the used targets.</strong>")
  
  
  event4 <- observeEvent(input$uploadInfo, {
    output$intro1 <- renderText(uploadInfo())
  })
  
  event5 <- observeEvent(input$outlayersInfo, {
    output$intro1 <- renderText(outlayersInfo())
  })
  
  event6 <- observeEvent(input$targetInfo, {
    output$intro1 <- renderText(targetsInfo())
  })
  
  output$checkbox <- renderUI({
  choice <- rv$genes
  rv$refs <- checkboxGroupInput("checkbox","Select referencegene(s) 3 max.", choices = choice, selected = choice[1])
  })
  
  event7 <- observeEvent(input$RefButton, {
    if(length(input$checkbox)==1){
      rv$normDf <- normalize1ref(rv$rawDf,input$checkbox[1])
      normText <- paste0("<h4><font color=\"#18B22D\"><br>normalization is done with ",input$checkbox[1], " as reference gene.</font></h4>")
    } else if(length(input$checkbox)==2) {
      rv$normDf <- normalize2ref(rv$rawDf,input$checkbox[1],input$checkbox[2])
      normText <- paste0("<h4><font color=\"#18B22D\"><br>normalization is done with ",input$checkbox[1], " and ",input$checkbox[2], " as reference genes.</font></h4>")
    } else if(length(input$checkbox)==3) {
      rv$normDf <- normalize3ref(rv$rawDf,input$checkbox[1],input$checkbox[2],input$checkbox[3])
      normText <- paste0("<h4><font color=\"#18B22D\"><br>normalization is done with ",input$checkbox[1], " , ",input$checkbox[2], " and ",input$checkbox[3], " as reference genes.</font></h4>")
    } else {
      rv$normDf <- NULL
      normText <- paste0("<h3><font color=\"#fa461e\"><center><br><br><br>your chosen number of reference genes (", length(input$checkbox),") <br>  is too high for this app.</center></font></h3>")
      
      

    }
    output$refText <- renderText(normText)
    
    output$refTable <- renderTable({   
     rv$normDf
      
    }, rownames = T)
    #output$targets <- renderText(paste0("Chosen reference genes: ", rv$refs))
  })
  

  output$checkbox2 <- renderUI({
    if(input$Norm2Sample == TRUE){
      choice <- unique(rv$rawDf$Sample)
      sampleText = "Choose sample you want to normalize to."
    } else {
      choice <- NULL
      sampleText = NULL
    }
    rv$sam <- checkboxGroupInput("checkbox2",sampleText , choices = choice, selected = NULL)
  })
  output$normtext <- renderText("<hr hr style=\"height:3px;border-width:0;color:#FAEF07;background-color:#FAEF07\"><strong>Average technical replicates and optionaly normalize to reference sample.</strong>")
  
  event8 <- observeEvent(input$normButton, {
    if(input$Norm2Sample == TRUE){
      refSample <- input$checkbox2[1]
    } else {
      refSample <- "None"
    }
    rv$data2plot <- AverageTRs(rv$normDf, refSample)
    output$refTable <- renderTable({
      rv$data2plot
    }, rownames = F)
    
  })
  event9 <- observeEvent(input$refGeneInfo, {
    output$intro2 <- renderText(refgeneInfo())
  })
  
  event10 <- observeEvent(input$refSampleInfo, {
    output$intro2 <- renderText(refsampleInfo())
  })
  
  ## rv$data2plot contains the data to be plotted.
  
}

