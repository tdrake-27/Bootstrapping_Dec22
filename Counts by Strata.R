library(boot)
library(bootstrap)
library(dplyr)
library(readxl)
library(rlang)
library(tidyverse)
library(mosaic)



# Clean up data, remove geese/coots, specify DABB/DIVE?AllDucks
Dec_22_R <- read_excel("Dec_22_R.xls")

Remove_geese_coot <- c("AMCO","CAGO","DEADDU","DEADGO","DKGO","GOLD","GWFG","LESG","LIGO","SACR","LSGO")

DABB <- c("ABDU","AMWI","AGWT","BBWD","BWTE","CITE","DABB","FUWD","GADW","MALL","NOPI","NSHO","TEAL","WODU")

DIVE <- c("BAGO","BUFF","COGO","CANV","DIVE","GRSC","HOME","LESC","MERG","RBME","RNDU","REDH","SCAU","RUDU")

Dec_22_Ducks <- Dec_22_R %>%
  filter(!(Species %in% Remove_geese_coot))
Master_df <- read_excel("11_03_2022_Ten Percent Chosen.xlsx")

Dec_22_DIVE <- Dec_22_Ducks %>%
  filter(!Species %in% DABB)

Dec_22_DABB <- Dec_22_Ducks %>%
  filter(!Species %in% DIVE)


Dec_22_total_DABB <- sum(Dec_22_DABB$Count)

Dec_22_total_DIVE <- sum(Dec_22_DIVE$Count)

Dec_22_total_AllDucks <- sum(Dec_22_Ducks$Count)

Dec_22_unique_count <- length(unique(Dec_22_Ducks$Transect))



# Summing the count in DABB
total_EOY_DABB <- Dec_22_DABB %>%
  filter(`Expert_Opinion` == "Yes") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))


total_EON_DABB <- Dec_22_DABB %>%
  filter(`Expert_Opinion` == "No") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

total_BT_DABB <- Dec_22_DABB %>%
  filter(`Stratum` == "BT") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

total_LO_DABB <- Dec_22_DABB %>%
  filter(`Stratum` == "LO") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

total_LR_DABB <- Dec_22_DABB %>%
  filter(`Stratum` == "LR") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

# Summing the count in DIVE

total_EOY_DIVE <- Dec_22_DIVE %>%
  filter(`Expert_Opinion` == "Yes") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))


total_EON_DIVE <- Dec_22_DIVE %>%
  filter(`Expert_Opinion` == "No") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

total_BT_DIVE <- Dec_22_DIVE %>%
  filter(`Stratum` == "BT") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

total_LO_DIVE <- Dec_22_DIVE %>%
  filter(`Stratum` == "LO") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))

total_LR_DIVE <- Dec_22_DIVE %>%
  filter(`Stratum` == "LR") %>%
  summarise(sum_ducks = sum(Count, na.rm = TRUE))
