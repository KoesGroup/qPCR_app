library(tidyverse)
library(readxl)


fetchRaw <- function(dataDir){
  ## reads files from a given directory and merges all the data to a single dataframe.
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
  ## calculates difference between highest and lowest value between
  ## technical replicates, returns a list of the ones exceeding a 
  ## user given value. 
  outLayers <- total %>%
    group_by(Sample,Target) %>%
    mutate("CTmean" = mean(CT), "CTdif"= max(CT)-min(CT)) %>%
    filter(CTdif > maxDif) %>%
    ungroup() %>%
    arrange(CTmean) %>%
    select(File,Sample,Target,CT, CTdif)
  return(outLayers)
}


TargetValues <- function(total){
  ## Returns for each target mean, min, 1st quartile, median, 3rd quartile and max CT value.
  ## take note, in case a sample target combination gives no CT value, it is set to 41.
  total[is.na(total)] <- 41
  quartiles <- total %>%
    group_by(Target) %>%
    mutate(meanCT = mean(CT), minCT = min(CT), firstCT = quantile(CT, 0.25), medianCT = quantile(CT, 0.5), thirdCT = quantile(CT, 0.75), maxCT = max(CT)) %>%
    ungroup() %>%
    select(Target, meanCT, minCT, firstCT, medianCT, thirdCT, maxCT) %>%
    distinct()
  return(quartiles)
}

normalize <- function(total,ref){
  ## normalizes CT values to expression values to a user chosen reference gen.
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


## geometric average: exp(mean(log(x)))   