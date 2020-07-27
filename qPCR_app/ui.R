library(shiny)
library(shinyFiles)
library(DT)
library(shinyjs)

ui <- fluidPage(
  titlePanel("QPCR Analysis"),
  tabsetPanel(
    tabPanel("Input",
             sidebarLayout(
               sidebarPanel(style = "background: #ECF5FF; border: #ECF5FF",
                 fileInput("csvs",
                           label="Choose 1 or more xlsx or xls files",
                           multiple = TRUE),
                 
                 actionButton("goButton", "upload", 
                              style="color: #fff; background-color: #CC0000; border-color: #CC0000; height:60px; width:130px"),
                 actionButton("uploadInfo", "info", 
                              style="color: #050505; background-color: #F1D5D3; border-color: #F1D5D3; height:60px; width:65px"),
                 htmlOutput("border1"),
                 
                 sliderInput("OutTrash", "max difference between technical replicates.", min = 0.00, max = 5.00, value = 0.50, step = 0.01),
                 actionButton("outButton", "outleyers", 
                              style="color: #fff; background-color: #2EAF2E; border-color: #2EAF2E; height:60px; width:130px"),
                 actionButton("outlayersInfo", "info", 
                              style="color: #050505; background-color: #C9EEC4; border-color: #C9EEC4; height:60px; width:65px"),
                 htmlOutput("border2"),
                 br(),
                 actionButton("TargetButton", "Targets", 
                              style="color: #fff; background-color: #1C1CE3; border-color: #1C1CE3; height:60px; width:130px"),
                 actionButton("targetInfo", "info", 
                              style="color: #050505; background-color: #C6CEEC; border-color: #C6CEEC; height:60px; width:65px")
                 
                 
                 
                 
                 
               ),
               #h4(htmlOutput("text1")),
               mainPanel(useShinyjs(),
                 wellPanel(
                           htmlOutput("intro1"),id = "wellPanelId1"
                 ),
                 
                 tableOutput("contents")
               )
             )
    ),
   tabPanel("Normalization",
             sidebarLayout(
               sidebarPanel(style = "background: #FCFDE6; border: #FCFDE6",
                            uiOutput("checkbox"),
                            actionButton("RefButton", "submit", 
                                         style="color: #fff; background-color: #F5A015; border-color: #F5A015; height:60px; width:130px"),
                            actionButton("refGeneInfo", "info", 
                                         style="color: #050505; background-color: #FCE0B3; border-color: #FCE0B3; height:60px; width:65px"),
                            br(),
                            actionButton("download_ref_df", "Download dCt Table"),
                           
                            
                            conditionalPanel( condition = "input.RefButton !==0",
                                              htmlOutput("normtext"),
                                              br(),
                                              checkboxInput("Norm2Sample", label = "Normalize to sample", value = FALSE),
                                              
                                              uiOutput("checkbox2"),
                                              br(),
                                              actionButton("normButton", "submit", 
                                                           style="color: #fff; background-color: #AB2DC4; border-color: #AB2DC4; height:60px; width:130px"),
                                              actionButton("refSampleInfo", "info", 
                                                           style="color: #050505; background-color: #EFBEF9; border-color: #EFBEF9; height:60px; width:65px"),
                                              br(),
                                              
                                              conditionalPanel( condition = "input.normButton !==0",
                                                                br(),
                                                                actionButton("plotButton2", "Plot", 
                                                                             style="color: #050505; background-color: #fbe6ff; border-color: #fbe6ff; height:60px; width:130px")
                                              ),
                                              br(),
                                              actionButton("download_norm_df", "Download Normalized Data Table")
                                              )
                            
                                                                                               
               ),
               mainPanel(useShinyjs(),
                 wellPanel(#style = "background: #D1FFD1; border: #D1FFD1",
                           htmlOutput("intro2"),id = "wellPanelId2"
                 ),
                 
                 htmlOutput("refText"),
                 tableOutput("refTable"),
                 plotOutput("rawPlot", height = 800)
                 
                 
               )
             )),
    tabPanel("Basic plot",
             sidebarLayout(
               sidebarPanel(style = "background: #E2FFD4; border: #E2FFD4",
                            htmlOutput("plotText1"),
                            radioButtons("TarSamBox","" , choices = c("Targets", "Samples"), selected = "Targets"),
                            htmlOutput("GreenBand2"),
                            checkboxInput("Coloroptions", label = "show more colors.", value = FALSE),
                            uiOutput("checkbox5"),
                            htmlOutput("plotText2"),
                            radioButtons("SubsetBox","Subset data?" , choices = c("no"=0, "subset Targets"=1, "subset Samples"=2, "subset Both"=3), selected = 0),
                            uiOutput("checkbox3"),
                            uiOutput("checkbox4"),
                            br(),
                            
                            conditionalPanel( condition = "input.normButton !==0",
                                              htmlOutput("GreenBand"), 
                                              actionButton("plotButton3", "Plot", 
                                                           style="color: #fff; background-color: #CC0000; border-color: #CC0000; height:60px; width:130px"),
                
                                              actionButton("basicplotinfo", "info", 
                                                           style="color: #050505; background-color: #F1D5D3; border-color: #F1D5D3; height:60px; width:65px")
                            )
                            

             
             
             
             
             
             
             
             
             ),
             mainPanel(useShinyjs(),
                       wellPanel(#style = "background: #D1FFD1; border: #D1FFD1",
                         htmlOutput("intro3"),id = "wellPanelId3"
                       ),
                       plotOutput("basicPlot", height = 800),
                       
                       tableOutput("plotTable")
             
             
             
             
             
             )))
  ))