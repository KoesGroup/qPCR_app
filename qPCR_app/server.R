library(shiny)
library(shinyFiles)
library(tidyverse)
library(data.table)
library(RColorBrewer)
library(xlsx)
library(readxl)

source("FetchData.R")
source("infoFile.R")

server <- function(input, output, session) {
  output$intro1 <- renderText("info box. Upon clicking on an info button, information will be displayed here.")
  rv <- reactiveValues(df = NULL, rawDf = NULL, targets = NULL, genes = NULL, refs = NULL, normDf = NULL,
                       samps = NULL, sam=NULL, sampleText = NULL, data2plot=NULL, sam2 = NULL, expDesignDF = NULL)  
  
  
  # set default colors for the wellpanels.
  runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                ", "wellPanelId1", "#ECF5FF"))
  
  runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                ", "wellPanelId2", "#FCFDE6"))
  
  runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId3", "#E2FFD4"))
  
  
  event1 <- observeEvent(input$goButton, {   #submit input counts.txt
    output$intro1 <- renderText("Info box")
    runjs(sprintf("
            document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId1", "#ECF5FF"))
    
    
    # upload and read the raw data files   
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
  
  ## show the list of samples with high technical variation 
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
  
  ## show list of targets
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
  
  ## Visualize the additional information on tab 1
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
  
  ## checkbox for chosing reference gene
  output$checkbox <- renderUI({
    choice <- rv$genes
    rv$refs <- checkboxGroupInput("checkbox","Select referencegene(s) 3 max.", choices = choice, selected = choice[1])
  })
  
  ##  get table of normalized values. Using one, two or three reference genes.
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
    
    if(is.null(rv$df)){
      output$knowNothing <-  renderImage({
        list(src = "knowNothing-gif.gif")
      }, deleteFile = F)
    }else{
      output$knowNothing <- NULL
    }
    
    output$refText <- renderText(normText)
    
    output$refTable <- renderTable({   
      rv$normDf
      
    }, rownames = T)
    
    output$rawPlot <- NULL #remove the existing plot from UI if the table is printed again after the data is plotted
    
  })
  
  
  
  # Show list of samples if requested, for chosing a reference sample.
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
    }
  })
  output$normtext <- renderText("<hr hr style=\"height:3px;border-width:0;color:#FAEF07;background-color:#FAEF07\"><strong>Average technical replicates and optionaly normalize to reference sample.</strong>")
  
  ## get technical averages and if requested normalize to reference sample
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
    
    output$rawPlot <- NULL #remove the existing plot from UI if the table is printed again after the data is plotted
    
  })
  output$spaceje1 <- renderText("<div style=\"line-height:25%;\"><br></div>")
  output$spaceje2 <- renderText("<div style=\"line-height:25%;\"><br></div>")
  output$spaceje3 <- renderText("<div style=\"line-height:25%;\"><br></div>")
  
  ## Show additional info on tab 2
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
  
  ##   
  event11 <- observeEvent(input$plotButton1, {
    
    output$refTable <- NULL
    
    output$rawPlot <- renderPlot({
      
      df <- rv$normDf
      ggplot(df, aes(x = Sample, y = dCt))+
        geom_col(aes(fill = Sample))+
        facet_grid(Target~.)
      
    })
    
  })
  
  ## 
  event12 <- observeEvent(input$plotButton2, {
    output$refTable <- NULL
    output$refText <- NULL
    
    output$rawPlot <- renderPlot({
      
      df <- rv$data2plot
      
      df <- df %>% filter(Target != input$checkbox )
      
      ggplot(df, aes(x = Sample, y = ddCt))+
        geom_col(aes(fill = Sample))+
        facet_grid(Target~.)+
        ggtitle(paste0("Relative expression of the genes using ", input$checkbox, " as reference"))+
        theme(plot.title = element_text(size = 16))
    })
  })
  
  eventDownloadRefData <- observeEvent(input$download_ref_df,{
    df <- rv$normDf
    write.xlsx(df, "qPCR_dCt_data.xlsx", sheetName = "Sheet1", 
               col.names = TRUE, row.names = FALSE, append = FALSE)
  })  
  
  
  eventDownloadNormData <- observeEvent(input$download_norm_df,{
    df <- rv$data2plot
    write.xlsx(df, "qPCR_norm_data.xlsx", sheetName = "Sheet1", 
               col.names = TRUE, row.names = TRUE, append = FALSE)
  }) 
  
  output$plotText1 <- renderText("<strong>In the plot, would you like the Targets to be grouped or the Samples.</strong>")
  output$plotText2 <- renderText("<strong><hr style=\"height:3px;border-width:0;color:#73E53E;background-color:#73E53E\"><br>Choose weather you want to plot the complete dataset or a subset.</strong>")
  
  ## set the choices for the colors of the plots.
  output$checkbox5 <- renderUI({
    if(input$Coloroptions == 0){
      colorlist <- NULL 
    } else if(input$Coloroptions == 1){colorlist <- c("Blues","Greens","Greys","Oranges","Purples","Reds")
    radioButtons("checkbox5","single color choices", choices = colorlist, selected = colorlist[1])
    } else if(input$Coloroptions == 2){colorlist <- c("blue & green"="BuGn","blue & purple"="BuPu","green & blue"="GnBu","orange & red"="OrRd","purple & blue"="PuBu","purple & red"="PuRd","red & purple"="RdPu")
    radioButtons("checkbox5","two color choices", choices = colorlist, selected = colorlist[1])
    } else if(input$Coloroptions == 3){colorlist <- c("purple, blue & green"="PuBuGn","yellow, green & blue"="YlGnBu","yellow, orange & brown"="YlOrBr","yellow, orange & red"="YlOrRd")
    radioButtons("checkbox5","three color choices", choices = colorlist, selected = colorlist[1])
    } else if(input$Coloroptions == 4){colorlist <- c("Set1","Set2","Set3","Pastel1", "Pastel2","Dark2","Accent")
    radioButtons("checkbox5","multi color choices", choices = colorlist, selected = colorlist[1])}
  })
  
  ## Show list of the used sample, to make selection
  output$checkbox4 <- renderUI({ 
    if (input$SubsetBox == 2 || input$SubsetBox == 3){
      choice <- unique(rv$rawDf$Sample)
      sampleText = "Choose sample(s)."
      if(is.null(choice)){
        rv$sam2 <- NULL
      } else {
        rv$sam2 <- checkboxGroupInput("checkbox3",sampleText , choices = choice, selected = choice[1])
      }
    } else {
      rv$sam2 <- NULL
    }
  })  
  
  ## Show list of used targets, to make a selection
  output$checkbox3 <- renderUI({ 
    if (input$SubsetBox == 1 || input$SubsetBox == 3){
      choice2 <- rv$genes
      sampleText2 = "Choose Target(s)."
      if(is.null(choice2)){
        rv$Tar2 <- NULL
      } else {
        rv$Tar2 <- checkboxGroupInput("checkbox4",sampleText2 , choices = choice2, selected = choice2[1])
      }
    } else {
      rv$Tar2 <- NULL
    }
  })
  
  output$GreenBand <- renderText("<hr style=\"height:3px;border-width:0;color:#73E53E;background-color:#73E53E\">")
  output$GreenBand2 <- renderText("<hr style=\"height:3px;border-width:0;color:#73E53E;background-color:#73E53E\">")
  
  # Change wellpanel color back to default
  event14 <- observeEvent(input$plotButton3, {
    output$intro3 <- renderText("Info Box.")
    runjs(sprintf("
        document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId3", "#E2FFD4"))
    
    # r4un the data subset function from the functions file.
    plotdf <- getPlotData(rv$data2plot, input$checkbox4, input$checkbox3, input$SubsetBox)
    
    # visualize the subsetted table
    output$plotTable <- renderTable({   
      plotdf
    }, rownames = T) 
    
    
    # choice of color pallet
    if(input$Coloroptions == 0){
      colorChoice <- "Set1" 
    } else { 
      colorChoice <- input$checkbox5
    }
    
    # build the plot either grouped by target or sample
    if(input$TarSamBox == "Targets"){
      colourCount = length(unique(plotdf$Sample))
      getPalette = colorRampPalette(brewer.pal(8, colorChoice))
      p <- ggplot(data=plotdf, aes(x=Target, y= ddCt, fill=Sample)) +
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        scale_fill_manual(values = getPalette(colourCount)) +
        geom_errorbar(aes(ymin=ddCt-CTstd, ymax=ddCt+CTstd), width=.2,
                      position=position_dodge(.9)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    } else {
      colourCount = length(unique(plotdf$Target))
      getPalette = colorRampPalette(brewer.pal(8, colorChoice))
      p <- ggplot(data=plotdf, aes(x=Sample, y= ddCt, fill=Target)) +
        geom_bar(stat="identity", color="black", position=position_dodge()) +
        scale_fill_manual(values = getPalette(colourCount)) +
        geom_errorbar(aes(ymin=ddCt-CTstd, ymax=ddCt+CTstd), width=.2,
                      position=position_dodge(.9)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }
    
    #visualize the plot
    output$basicPlot <- renderPlot({
      p
    })
    
  })
  
  
  
  
  
  # Show plot info in the info box
  event15 <- observeEvent(input$basicplotinfo, {
    output$intro3 <- renderText(basicPlotInfo())
    runjs(sprintf("
                  document.getElementById('%s').style.backgroundColor = '%s';
                  ", "wellPanelId3", "#F1D5D3"))
  })
  
  eventExpDesign <- observeEvent(input$expDesignGo, {
    req(input$expDesign)
    expDesignFile <- input$expDesign
    rv$expDesignDF <- read_excel(expDesignFile$datapath)
    
    output$expDesignTable <- renderTable(rv$expDesignDF)
    
    normDF <- rv$data2plot
    expDesignDF <- rv$expDesignDF
    
    #output$targetGenes <- renderTable(normDF)
    targetList <- unique(factor(normDF$Target))
    axisList <- colnames(expDesignDF[,which(names(expDesignDF) != "Sample")])
    
    updateCheckboxGroupInput(session, "targetChoice", label = "Select target:", choices = targetList, selected = targetList[1])
    updateRadioButtons(session, "xAxisChoice", label = "Select variable in X axis:", choices = axisList, selected = axisList[1])
   observe({ 
    fillList <- axisList[which(factor(axisList) != input$xAxisChoice)]
    updateRadioButtons(session, "fillChoice", label = "Select fill:", choices = fillList, selected = fillList[1])
    print(input$xAxisChoice)
   })

})
  
  eventAdvPlot <- observeEvent(input$plotButton4, {
    
    output$expDesignTable <- NULL
    
    normDF <- rv$data2plot
    expDesignDF <- rv$expDesignDF
    
    #output$targetGenes <- renderTable(normDF)
    targetList <- unique(factor(normDF$Target))
    
    print(input$plotTarget)
    targetsQuote <- shQuote(input$targetChoice, type = "cmd")
    
    selTargets <- sprintf(paste0("Target", " == %s"), targetsQuote)
    print(selTargets)
    selTargetsCondition <- paste(factor(selTargets), collapse = " | ")
    print(selTargetsCondition)
    
    xAxis <- input$xAxisChoice
    print(xAxis)
    
    fillVar <- input$fillChoice
    print(fillVar)
 
    df <- left_join(normDF, expDesignDF, by = "Sample") %>% drop_na() %>% filter_(selTargetsCondition)
    #df[1] <- NULL
    errorGroup <- colnames(expDesignDF[,which(!names(expDesignDF) %in% c("Sample", fillVar,xAxis))])
    print(errorGroup)
    dfM <-   df %>% group_by_at(vars(-Sample, -ddCt, -CTstd, -errorGroup)) %>%
      mutate(ddCtmean = mean(ddCt), ddCtSD = sd(ddCt)) %>% 
      ungroup()
  
    output$plotTable2  <- renderTable(dfM) 

  
   output$advPlot <- renderPlot(
      ggplot(dfM, aes(x = get(xAxis), y = ddCtmean, fill = get(fillVar)))+
       geom_col(position = position_dodge())+ #needs fill to properly dodge
        geom_errorbar(aes(ymin=ddCtmean-ddCtSD, ymax=ddCtmean+ddCtSD), position = position_dodge())
   )
    
    #updateRadioButtons(session, "fillChoice", label = "Select fill:", choices = fillList, selected = fillList[1])
   # print(input$xAxisChoice)
    
  })
  
}



