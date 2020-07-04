library(tidyverse)
library(readxl)


fetchRaw <- function(dataDir){
  rawdata <- list.files(dataDir)
  total <- data.frame()
  for(f in rawdata){
    path2file = paste0(dataDir,f)
    PCRdata <- read_excel(path2file, sheet = "Results", col_names = TRUE, col_types = NULL, na = "", skip =43 )
    PCRsubset <- PCRdata %>%
      select("Sample Name","Target Name","CT") %>%
      mutate("New"=f) %>%
      as.data.frame()
    colnames(PCRsubset) <- c("Sample","Target","CT","File")
    PCRsubset <- PCRsubset[complete.cases(PCRsubset),]
    PCRsubset$CT <- as.double(PCRsubset$CT)
    if(nrow(total) == 0){
      total <- PCRsubset
    } else {
    total <- merge(total,PCRsubset,all = T)
    }
  }
  return(total)
}


GetOutlayers <- function(total, maxDif){
  lijst2 <- total %>%
    group_by(Sample,Target) %>%
    mutate("CTmean" = mean(CT), "CTdif"= max(CT)-min(CT)) %>%
    filter(CTdif > maxDif) %>%
    ungroup() %>%
    arrange(CTmean) %>%
    select(File,Sample,Target,CT, CTdif)
}
 
normalize <- function(total,ref){
  refList <- total %>%
    select(Sample,Target,CT) %>%
    group_by(Sample,Target) %>%
    mutate("CTmean" = mean(CT)) %>%
    ungroup() %>%
    filter(Target == ref) %>%
    select(Sample,CTmean) %>%
    distinct()
  WithRef <- merge(total, refList)
  norm <- WithRef %>%
    mutate("CTdif" = 2^(CTmean - CT)) %>%
    group_by(Sample,Target) %>%
    mutate(CTnorm = mean(CTdif), CTstd = sd(CTdif)) %>%
    ungroup() %>%
    select(Sample,Target,CTnorm,CTstd) %>%
    distinct()
  return(norm)
}





