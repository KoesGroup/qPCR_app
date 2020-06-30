library(tidyverse)
library(readxl)

sand <- read_excel("~/Desktop/2019-09-16_SAND_CVD-FC_9488-9489_2uM cDNA.xls", 
                                                         skip = 43)

fro <- read_excel("~/Desktop/2020-02-06_FRO2_FC-CVD_ROOTS_9803-9817.xls", 
                                                      skip = 43)

fro <- fro %>% select(`Sample Name`, `Target Name`, `Ct Mean`) %>% drop_na()
sand <- sand %>% select(`Sample Name`, `Target Name`, `Ct Mean`) %>% drop_na()

rawdata <- rbind(fro, sand)

########
  
fro <- data.frame(fro$`Sample Name`,fro$`Target Name`, fro$`Ct Mean`)  



names <- data.frame(name = c("fro","sand"))

datalist <- list()

lengthNames <- length(names$name)


for (i in 1:lengthNames){


  print(names[i,])
  df <- read_excel(normalizePath(file.path(paste0(names[i,],".xls"))), 
             skip = 43)
  print(df)
  
  
}


