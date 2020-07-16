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
  
  runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                ", "wellPanelId1", "#ECF5FF"))
  
  runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                ", "wellPanelId2", "#FCFDE6"))
  
  event1 <- observeEvent(input$goButton, {   #submit input counts.txt
    output$intro1 <- renderText("Info box")
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId1", "#ECF5FF"))
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
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId1", "#ECF5FF"))
    MaxDif <- input$OutTrash
    output$contents <- renderTable({   
      GetOutlayers(rv$rawDf, maxDif=MaxDif)
    }, rownames = T)
  }) 
  
  event3 <- observeEvent(input$TargetButton, {
    output$intro1 <- renderText("Info box")
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                                                           ", "wellPanelId1", "#ECF5FF"))
    geneList <- TargetValues(rv$rawDf) #can be removed? already in event1
    rv$genes <- geneList$Target #can be removed?
    output$contents <- renderTable({   
      geneList
    }, rownames = F)
  })
  
  
  output$border1 <- renderText("<hr style=\"height:3px;border-width:0;color:#9E9E9C;background-color:#9E9E9C\">")
  
  output$border2 <- renderText("<hr style=\"height:3px;border-width:0;color:#9E9E9C;background-color:#9E9E9C\"><strong>List of the used targets.</strong>")
  
  
  event4 <- observeEvent(input$uploadInfo, {
    output$intro1 <- renderText(uploadInfo())
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId1", "#F1D5D3"))
  })
  
  event5 <- observeEvent(input$outlayersInfo, {
    output$intro1 <- renderText(outlayersInfo())
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId1", "#C9EEC4"))
  })
  
  event6 <- observeEvent(input$targetInfo, {
    output$intro1 <- renderText(targetsInfo())
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId1", "#C6CEEC"))
  })
  
  output$checkbox <- renderUI({
    choice <- rv$genes
    rv$refs <- checkboxGroupInput("checkbox","Select referencegene(s) 3 max.", choices = choice, selected = choice[1])
  })
  
  event7 <- observeEvent(input$RefButton, {
    output$intro2 <- renderText("Info box")
    runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId2", "#FCFDE6"))
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
      if(is.null(choice)){
        rv$sam <- NULL
      } else {
      rv$sam <- radioButtons("checkbox2",sampleText , choices = choice, selected = choice[1])
      }
    } else {
      rv$sam <- NULL
      # choice <- ""    #NULL
      #  sampleText = NULL
    }
    #rv$sam <- radioButtons("checkbox2",sampleText , choices = choice, selected = "")
  })
  output$normtext <- renderText("<hr hr style=\"height:3px;border-width:0;color:#FAEF07;background-color:#FAEF07\"><strong>Average technical replicates and optionaly normalize to reference sample.</strong>")
  
  event8 <- observeEvent(input$normButton, {
    output$intro2 <- renderText("Info box")
    runjs(sprintf("
                  document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId2", "#FCFDE6"))
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
    runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId2", "#FCE0B3"))
  })
  
  event10 <- observeEvent(input$refSampleInfo, {
    output$intro2 <- renderText(refsampleInfo())
    runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId2", "#EFBEF9"))
  })
  
  event11.1 <- 
    
    event11 <- observeEvent(input$plotButton1, {
      
      output$refTable <- NULL
      
      output$rawPlot <- renderPlot({
        
        df <- rv$normDf
        ggplot(df, aes(x = Sample, y = CTdif))+
          geom_col(aes(fill = Sample))
      })
      
    })
  
  event12 <- observeEvent(input$plotButton2, {
    output$refTable <- NULL
    output$rawPlot <- renderPlot({
      
      df <- rv$data2plot
      ggplot(df, aes(x = Sample, y = CTnorm))+
        geom_col(aes(fill = Sample))
    })
  })
  
  
  ## rv$data2plot contains the data to be plotted.
  
  
}