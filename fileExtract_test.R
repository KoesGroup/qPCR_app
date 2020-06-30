library(tidyverse)
library(readxl)

sand <- read_excel("~/Desktop/2019-09-16_SAND_CVD-FC_9488-9489_2uM cDNA.xls", 
                                                         skip = 43)

fro <- read_excel("~/Desktop/2020-02-06_FRO2_FC-CVD_ROOTS_9803-9817.xls", 
                                                      skip = 43)

fro <- fro %>% select(`Sample Name`, `Target Name`)
