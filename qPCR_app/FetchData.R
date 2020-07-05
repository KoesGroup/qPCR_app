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

normalize1ref <- function(total,ref){
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


normalize2ref <- function(total,ref1,ref2){
refList1 <- total %>%
  select(Sample,Target,CT) %>%
  group_by(Sample,Target) %>%
  mutate("CTmean1" = mean(CT)) %>%
  ungroup() %>%
  filter(Target == ref1) %>%
  select(Sample,CTmean1) %>%
  distinct()
refList2 <- total %>%
  select(Sample,Target,CT) %>%
  group_by(Sample,Target) %>%
  mutate("CTmean2" = mean(CT)) %>%
  ungroup() %>%
  filter(Target == ref2) %>%
  select(Sample,CTmean2) %>%
  distinct()
refList <- merge(refList1,refList2) %>%
  group_by(Sample) %>%
  mutate(geoMean = exp(mean(log(c(CTmean1,CTmean2))))) %>%
  ungroup() %>%
  select(Sample, geoMean)
norm <- merge(total, refList) %>%
  mutate("CTdif" = 2^(geoMean - CT)) %>%
#  group_by(Sample,Target) %>%
#  mutate(CTnorm = mean(CTdif), CTstd = sd(CTdif)) %>%
#  ungroup() %>%
  select(Sample,Target,CTdif) %>%
  distinct()
}


normalize3ref <- function(total,ref1,ref2,ref3){
  refList1 <- total %>%
    select(Sample,Target,CT) %>%
    group_by(Sample,Target) %>%
    mutate("CTmean1" = mean(CT)) %>%
    ungroup() %>%
    filter(Target == ref1) %>%
    select(Sample,CTmean1) %>%
    distinct()
  refList2 <- total %>%
    select(Sample,Target,CT) %>%
    group_by(Sample,Target) %>%
    mutate("CTmean2" = mean(CT)) %>%
    ungroup() %>%
    filter(Target == ref2) %>%
    select(Sample,CTmean2) %>%
    distinct()
  refList3 <- total %>%
    select(Sample,Target,CT) %>%
    group_by(Sample,Target) %>%
    mutate("CTmean3" = mean(CT)) %>%
    ungroup() %>%
    filter(Target == ref3) %>%
    select(Sample,CTmean3) %>%
    distinct()
  refList <- refList1 %>%
    merge(refList2) %>%
    merge(refList3) %>%
    group_by(Sample) %>%
    mutate(geoMean = exp(mean(log(c(CTmean1,CTmean2,CTmean3))))) %>%
    ungroup() %>%
    select(Sample, geoMean)
  norm <- merge(total, refList) %>%
    mutate("CTdif" = 2^(geoMean - CT)) %>%
    #  group_by(Sample,Target) %>%
    #  mutate(CTnorm = mean(CTdif), CTstd = sd(CTdif)) %>%
    #  ungroup() %>%
    select(Sample,Target,CTdif) %>%
    distinct()
}


AverageTRs <- function(norm, sample="None"){
  ## Returns a list of averages and stdevs of technical replicates. ea the same sample/target combinations.
  ## if requested normalized to a sample 
  if(sample=="None"){
    Averages <- norm %>%
      group_by(Sample, Target) %>%
      mutate(CTnorm = mean(CTdif), CTstd = sd(CTdif)) %>%
      ungroup() %>%
      select(Sample, Target, CTnorm, CTstd) %>%
      distinct()
  } else {
    refSample <- norm %>%
      filter(Sample == sample) %>%
      group_by(Sample, Target) %>%
      mutate(CTref = mean(CTdif)) %>%
      ungroup() %>%
      select(Target, CTref) %>%
      distinct()
    Averages <- normalized3 %>%
      merge(refSample) %>%
      mutate(CTDD=CTdif/CTref) %>%
      group_by(Sample,Target) %>%
      mutate(CTnorm = mean(CTDD), CTstd = sd(CTDD)) %>%
      ungroup() %>%
      select(Sample, Target, CTnorm, CTstd) %>%
      distinct()  
  }
  return(Averages)
}   
    

Average2groups <- function(sampleFile, averages){
  sampleData <- read_excel(sampleFile, col_names = TRUE, col_types = NULL, na = "")
  groupAverages <- Final %>%
    merge(sampleData) %>%
    group_by(Group, Target) %>%
    mutate(CTgroupMean = mean(CTnorm), CTgroupStdev = sd(CTnorm)) %>%
    ungroup() %>%
    select(Group, Target, CTgroupMean, CTgroupStdev) %>%
    distinct()
  return(groupAverages)
  }

## geometric average: exp(mean(log(x)))   