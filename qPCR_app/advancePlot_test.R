library(tidyverse)
library(readxl)

normData <- read_excel("qPCR_norm_data.xlsx")
expDesign <- read_excel("expDesignFRO2.xlsx")

df1 <- left_join(normData, expDesign, by = "Sample") %>% drop_na() %>% filter(Target == "FRO2")
df1[1] <- NULL
dfM <-   df1 %>% group_by_at(vars(-Sample, -ddCt, -CTstd)) %>% #not working yet
  mutate(ddCtmean = mean(ddCt), ddCtSD = sd(ddCt)) %>% 
  ungroup()


print(colnames(dfM))

xAxisChoice <- df1$copper

colnames(dfM[,which(names(dfM) != "copper")])

ggplot(dfM, aes(x = xAxisChoice, y = ddCtmean, fill = copper))+ #if no fill should we fill by Target?
  geom_col(position = position_dodge())+ #needs fill to properly dodge
  geom_errorbar(aes(ymin=ddCtmean-ddCtSD, ymax=ddCtmean+ddCtSD), position = position_dodge())


###############
targets <- c("SpSAND")

targetsQuote <- shQuote(targets, type = "cmd")

  
selTargets <- sprintf(paste0("Target", " == %s"), targetsQuote)
selTargets
selTargetsCondition <- paste(factor(selTargets), collapse = " | ")
selTargetsCondition

df <- left_join(normData, expDesign, by = "Sample") %>% drop_na() %>% filter_(selTargetsCondition)

unique(factor(normData$Target))
#############
