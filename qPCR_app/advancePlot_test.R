library(tidyverse)
library(readxl)

normData <- read_excel("qPCR_norm_data.xlsx")
expDesign <- read_excel("expDesignFRO2.xlsx")

df <- left_join(normData, expDesign, by = "Sample") %>% drop_na() %>% filter(Target == "FRO2")
df[1] <- NULL
dfM <-   df %>% group_by_at(vars(-Sample, -ddCt, -CTstd)) %>% #not working yet
  mutate(ddCtmean = mean(ddCt), ddCtSD = sd(ddCt)) %>% 
  ungroup()


print(colnames(dfM))

xAxisChoice <- df$copper

ggplot(dfM, aes(x = xAxisChoice, y = ddCtmean, fill = ecotype))+ #if no fill should we fill by Target?
  geom_col(position = position_dodge())+ #needs fill to properly dodge
  geom_errorbar(aes(ymin=ddCtmean-ddCtSD, ymax=ddCtmean+ddCtSD), position = position_dodge())




