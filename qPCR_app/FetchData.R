library(tidyverse)
library(readxl)

dataDir <- "Desktop/QPCR-app/data/"



rawdata <- list.files(dataDir)

rawdata
total <- data.frame()

for(f in rawdata){
  path2file = paste0(dataDir,f)
  print(path2file)
  PCRdata <- read_excel(path2file, sheet = "Results", col_names = TRUE, col_types = NULL, na = "", skip =43 )
  PCRsubset <- PCRdata %>%
    select("Sample Name","Target Name","CT") %>%
    as.data.frame()
  colnames(PCRsubset) <- c("Sample","Target","CT")
  PCRsubset <- PCRsubset[complete.cases(PCRsubset),]
  PCRsubset$CT <- as.double(PCRsubset$CT)
  print(nrow(PCRsubset))
  if(nrow(total) == 0){
    print("check")
    total <- PCRsubset
  } else {
  total <- merge(total,PCRsubset,all = T)
  }
}
str(PCRsubset)
total$CT <- as.double(total$CT)
total$Sample = substr(total$Sample,1,nchar(total$Sample)-2) ### only for Sara's samples
total <- total %>%
  select(Sample,Target,CT) %>%
  group_by(Sample,Target) %>%
  mutate("CTmean" = mean(CT))

head(total, 20)

PCRdata <- read_excel("Desktop/QPCR-app/data/A1010.2_P1.xls", sheet = "Results", col_names = TRUE, col_types = NULL, na = "", skip =43 )
colnames(PCRdata)
PCRsubset <- PCRdata %>%
  select("Sample Name","Target Name","CT")
PCRsubset1 <- PCRsubset[complete.cases(PCRsubset),]

PCRdata <- read_excel("Desktop/QPCR-app/data/M1.V30_P1.xls", sheet = "Results", col_names = TRUE, col_types = NULL, na = "", skip =43 )
colnames(PCRdata)
PCRsubset <- PCRdata %>%
  select("Sample Name","Target Name","CT")
colnames(PCRsubset) <- c("Sample","Target","CT")
PCRsubset2 <- PCRsubset[complete.cases(PCRsubset),]
PCRsubset3 <- merge(PCRsubset1,PCRsubset2, all = T)
PCRsubset %>%
  select(Sample,Target,CT) %>%
  group_by(Sample,Target) %>%
  mutate("New" = mean(CT))
mean(as.double(PCRsubset$CT))
 
PCRsubset$Sample = substr(PCRsubset$Sample,1,nchar(PCRsubset$Sample)-2)
head( PCRsubset)



