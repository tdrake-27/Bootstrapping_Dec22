library(boot)
library(bootstrap)
library(dplyr)
library(readxl)
library(rlang)
library(tidyverse)
library(mosaic)

unmasked_object1 <- stats:::var
unmasked_object2 <- stats:::sd

# Clean up data, remove geese/coots, specify DABB/DIVE?AllDucks
Dec_22_R <- read_excel("Dec_22_R.xls")

Remove_geese_coot <- c("AMCO","CAGO","DEADDU","DEADGO","DKGO","GOLD","GWFG","LESG","LIGO","SACR")

DABB <- c("ABDU","AMWI","AGWT","BBWD","BWTE","CITE","DABB","FUWD","GADW","MALL","NOPI","NSHO","TEAL","WODU")

DIVE <- c("BAGO","BUFF","COGO","CANV","DIVE","GRSC","HOME","LESC","MERG","RBME","RNDU","REDH","SCAU")

Dec_22_Ducks <- Dec_22_R %>%
  filter(!(Species %in% Remove_geese_coot))  ### Remove undesired species

Master_df <- read_excel("11_03_2022_Ten Percent Chosen.xlsx") ### Pull in master data set for transect lines

Dec_DIVE <- Dec_22_Ducks %>%
  filter(!Species %in% DABB)

Dec_DABB <- Dec_22_Ducks %>%
  filter(!Species %in% DIVE)

dec_total_DABB <- Dec_DABB %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

dec_total_DIVE <- Dec_DIVE %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

dec_total_AllDucks <- Dec_22_Ducks %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

unique_count <- length(unique(Dec_22_Ducks$Transect))

# Calculate mean, SD, SE, CV, Variance for R10% DABB/DIVE/AllDucks

## Random 10%
### DABB
d_mean_DABB <- mean(dec_total_DABB$total_ducks)
d_sd_DABB <- sd(dec_total_DABB$total_ducks)
d_ss_DABB <- length(dec_total_DABB$Transect)
d_SE_DABB <- d_sd_DABB / sqrt(d_ss_DABB)
d_CV_DABB <- (d_sd_DABB/d_mean_DABB)*100
cat("Standard Error - Dec DABB", d_SE_DABB, "\n")
cat("Coefficient of Variation - Dec DABB:", d_CV_DABB, "%\n")

d_pop_variance_DABB <- var(dec_total_DABB$total_ducks, na.rm = TRUE)

### DIVE
d_mean_DIVE <- mean(dec_total_DIVE$total_ducks)
d_sd_DIVE <- sd(dec_total_DIVE$total_ducks)
d_ss_DIVE <- length(dec_total_DIVE$Transect)
d_SE_DIVE <- d_sd_DIVE / sqrt(d_ss_DIVE)
d_CV_DIVE <- (d_sd_DIVE/d_mean_DIVE)*100
cat("Standard Error - Dec DIVE", d_SE_DIVE, "\n")
cat("Coefficient of Variation - Dec DIVE:", d_CV_DIVE, "%\n")

d_pop_variance_DIVE <- var(dec_total_DIVE$total_ducks, na.rm = TRUE)

### AllDucks
d_mean_AllDucks <- mean(dec_total_AllDucks$total_ducks)
d_sd_AllDucks <- sd(dec_total_AllDucks$total_ducks)
d_ss_AllDucks <- length(dec_total_AllDucks$Transect)
d_SE_AllDucks <- d_sd_AllDucks / sqrt(d_ss_AllDucks)
d_CV_AllDucks <- (d_sd_AllDucks/d_mean_AllDucks)*100
cat("Standard Error - Dec AllDucks", d_SE_AllDucks, "\n")
cat("Coefficient of Variation - Dec AllDucks:", d_CV_AllDucks, "%\n")

d_pop_variance_AllDucks <- var(dec_total_AllDucks$total_ducks, na.rm = TRUE)



## BT Watershed

BT_DABB <- Dec_DABB %>%
  filter(!(Stratum %in% c("LO", "LR")))

BT_total_DABB <- BT_DABB %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

BT_Count_DABB <- sum(BT_total_DABB$total_ducks)


### DABB
bt_mean_DABB <- mean(BT_total_DABB$total_ducks)
bt_sd_DABB <- sd(BT_total_DABB$total_ducks)
bt_ss_DABB <- length(BT_total_DABB$Transect)
bt_SE_DABB <- bt_sd_DABB / sqrt(bt_ss_DABB)
bt_CV_DABB <- (bt_sd_DABB/bt_mean_DABB)*100
cat("Standard Error - Dec DABB", bt_SE_DABB, "\n")
cat("Coefficient of Variation - Dec DABB:", bt_CV_DABB, "%\n")

bt_pop_variance_DABB <- var(BT_total_DABB$total_ducks, na.rm = TRUE)



BT_DIVE <- Dec_DIVE %>%
  filter(!(Stratum %in% c("LO", "LR")))

BT_total_DIVE <- BT_DIVE %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

BT_Count_DIVE <- sum(BT_total_DIVE$total_ducks)

### DIVE
bt_mean_DIVE <- mean(BT_total_DIVE$total_ducks)
bt_sd_DIVE <- sd(BT_total_DIVE$total_ducks)
bt_ss_DIVE <- length(BT_total_DIVE$Transect)
bt_SE_DIVE <- bt_sd_DIVE / sqrt(bt_ss_DIVE)
bt_CV_DIVE <- (bt_sd_DIVE/bt_mean_DIVE)*100
cat("Standard Error - Dec DIVE", bt_SE_DIVE, "\n")
cat("Coefficient of Variation - Dec DIVE:", bt_CV_DIVE, "%\n")

bt_pop_variance_DIVE <- var(BT_total_DIVE$total_ducks, na.rm = TRUE)


BT_AllDucks <- Dec_22_Ducks %>%
  filter(!(Stratum %in% c("LO", "LR")))

BT_total_AllDucks <- BT_AllDucks %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

BT_Count_AllDucks <- sum(BT_total_AllDucks$total_ducks)

### AllDucks
bt_mean_AllDucks <- mean(BT_total_AllDucks$total_ducks)
bt_sd_AllDucks <- sd(BT_total_AllDucks$total_ducks)
bt_ss_AllDucks <- length(BT_total_AllDucks$total_ducks)
bt_SE_AllDucks <- bt_sd_AllDucks / sqrt(bt_ss_AllDucks)
bt_CV_AllDucks <- (bt_sd_AllDucks/bt_mean_AllDucks)*100
cat("Standard Error - Dec AllDucks", bt_SE_AllDucks, "\n")
cat("Coefficient of Variation - Dec AllDucks:", bt_CV_AllDucks, "%\n")

d_pop_variance_AllDucks <- var(BT_total_AllDucks$total_ducks, na.rm = TRUE)


## LO Watershed
### DABB
LO_DABB <- Dec_DABB %>%
  filter(!(Stratum %in% c("BT", "LR")))

LO_total_DABB <- LO_DABB %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

LO_Count_DABB <- sum(LO_total_DABB$total_ducks)

### DABB
lo_mean_DABB <- mean(LO_total_DABB$total_ducks)
lo_sd_DABB <- sd(LO_total_DABB$total_ducks)
lo_ss_DABB <- length(LO_total_DABB$Transect)
lo_SE_DABB <- lo_sd_DABB / sqrt(lo_ss_DABB)
lo_CV_DABB <- (lo_sd_DABB/lo_mean_DABB)*100
cat("Standard Error - Dec LO DABB", lo_SE_DABB, "\n")
cat("Coefficient of Variation - Dec LO DABB:", lo_CV_DABB, "%\n")

lo_pop_variance_DABB <- var(LO_total_DABB$total_ducks, na.rm = TRUE)



LO_DIVE <- Dec_DIVE %>%
  filter(!(Stratum %in% c("BT", "LR")))

LO_total_DIVE <- LO_DIVE %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

LO_Count_DIVE <- sum(LO_total_DIVE$total_ducks)

### DIVE
lo_mean_DIVE <- mean(LO_total_DIVE$total_ducks)
lo_sd_DIVE <- sd(LO_total_DIVE$total_ducks)
lo_ss_DIVE <- length(LO_total_DIVE$Transect)
lo_SE_DIVE <- lo_sd_DIVE / sqrt(lo_ss_DIVE)
lo_CV_DIVE <- (lo_sd_DIVE/lo_mean_DIVE)*100
cat("Standard Error - Dec LO DIVE", lo_SE_DIVE, "\n")
cat("Coefficient of Variation - Dec LO DIVE:", lo_CV_DIVE, "%\n")

lo_pop_variance_DIVE <- var(LO_total_DIVE$total_ducks, na.rm = TRUE)


LO_AllDucks <- Dec_22_Ducks %>%
  filter(!(Stratum %in% c("LR", "BT")))

LO_total_AllDucks <- LO_AllDucks %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

LO_Count_AllDucks <- sum(LO_total_AllDucks$total_ducks)

### AllDucks
lo_mean_AllDucks <- mean(LO_total_AllDucks$total_ducks)
lo_sd_AllDucks <- sd(LO_total_AllDucks$total_ducks)
lo_ss_AllDucks <- length(LO_total_AllDucks$total_ducks)
lo_SE_AllDucks <- lo_sd_AllDucks / sqrt(lo_ss_AllDucks)
lo_CV_AllDucks <- (lo_sd_AllDucks/lo_mean_AllDucks)*100
cat("Standard Error - Dec  LO AllDucks", lo_SE_AllDucks, "\n")
cat("Coefficient of Variation - Dec LO AllDucks:", lo_CV_AllDucks, "%\n")

d_pop_variance_AllDucks <- var(LO_total_AllDucks$total_ducks, na.rm = TRUE)



## LR Watershed

### DABB
LR_DABB <- Dec_DABB %>%
  filter(!(Stratum %in% c("BT", "LO")))

LR_total_DABB <- LR_DABB %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

LR_Count_DABB <- sum(LR_total_DABB$total_ducks)

### DABB
lr_mean_DABB <- mean(LR_total_DABB$total_ducks)
lr_sd_DABB <- sd(LR_total_DABB$total_ducks)
lr_ss_DABB <- length(LR_total_DABB$Transect)
lr_SE_DABB <- lr_sd_DABB / sqrt(lr_ss_DABB)
lr_CV_DABB <- (lr_sd_DABB/lr_mean_DABB)*100
cat("Standard Error - Dec LR DABB", lr_SE_DABB, "\n")
cat("Coefficient of Variation - Dec LR DABB:", lr_CV_DABB, "%\n")

lr_pop_variance_DABB <- var(LR_total_DABB$total_ducks, na.rm = TRUE)



LR_DIVE <- Dec_DIVE %>%
  filter(!(Stratum %in% c("BT", "LO")))

LR_total_DIVE <- LR_DIVE %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

LR_Count_DIVE <- sum(LR_total_DIVE$total_ducks)

### DIVE
lr_mean_DIVE <- mean(LR_total_DIVE$total_ducks)
lr_sd_DIVE <- sd(LR_total_DIVE$total_ducks)
lr_ss_DIVE <- length(LR_total_DIVE$Transect)
lr_SE_DIVE <- lr_sd_DIVE / sqrt(lr_ss_DIVE)
lr_CV_DIVE <- (lr_sd_DIVE/lr_mean_DIVE)*100
cat("Standard Error - Dec LR DIVE", lr_SE_DIVE, "\n")
cat("Coefficient of Variation - Dec LR DIVE:", lr_CV_DIVE, "%\n")

lr_pop_variance_DIVE <- var(LR_total_DIVE$total_ducks, na.rm = TRUE)


LR_AllDucks <- Dec_22_Ducks %>%
  filter(!(Stratum %in% c("LO", "BT")))

LR_total_AllDucks <- LR_AllDucks %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

LR_Count_AllDucks <- sum(LR_total_AllDucks$total_ducks)

### AllDucks
lr_mean_AllDucks <- mean(LR_total_AllDucks$total_ducks)
lr_sd_AllDucks <- sd(LR_total_AllDucks$total_ducks)
lr_ss_AllDucks <- length(LR_total_AllDucks$total_ducks)
lr_SE_AllDucks <- lr_sd_AllDucks / sqrt(lr_ss_AllDucks)
lr_CV_AllDucks <- (lr_sd_AllDucks/lr_mean_AllDucks)*100
cat("Standard Error - Dec  LR AllDucks", lr_SE_AllDucks, "\n")
cat("Coefficient of Variation - Dec LR AllDucks:", lr_CV_AllDucks, "%\n")

lr_pop_variance_AllDucks <- var(LR_total_AllDucks$total_ducks, na.rm = TRUE)



## Yes - Expert Opinion

EOY_DABB <- Dec_DABB %>%
  filter(!(Expert_Opinion %in% "No"))

EOY_total_DABB <- EOY_DABB %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

EOY_Count_DABB <- sum(EOY_total_DABB$total_ducks)


### DABB
eoy_mean_DABB <- mean(EOY_total_DABB$total_ducks)
eoy_sd_DABB <- sd(EOY_total_DABB$total_ducks)
eoy_ss_DABB <- length(EOY_total_DABB$Transect)
eoy_SE_DABB <- eoy_sd_DABB / sqrt(eoy_ss_DABB)
eoy_CV_DABB <- (eoy_sd_DABB/eoy_mean_DABB)*100
cat("Standard Error - Dec DABB", eoy_SE_DABB, "\n")
cat("Coefficient of Variation - Dec DABB:", eoy_CV_DABB, "%\n")

eoy_pop_variance_DABB <- var(EOY_total_DABB$total_ducks, na.rm = TRUE)

### DIVE

EOY_DIVE <- Dec_DIVE %>%
  filter(!(Expert_Opinion %in% "No"))

EOY_total_DIVE <- EOY_DIVE %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

EOY_Count_DIVE <- sum(EOY_total_DIVE$total_ducks)

eoy_mean_DIVE <- mean(EOY_total_DIVE$total_ducks)
eoy_sd_DIVE <- sd(EOY_total_DIVE$total_ducks)
eoy_ss_DIVE <- length(EOY_total_DIVE$total_ducks)
eoy_SE_DIVE <- eoy_sd_DIVE / sqrt(eoy_ss_DIVE)
eoy_CV_DIVE <- (eoy_sd_DIVE/eoy_mean_DIVE)*100
cat("Standard Error - Dec eoy DIVE", eoy_SE_DIVE, "\n")
cat("Coefficient of Variation - Dec  eoy DIVE:", eoy_CV_DIVE, "%\n")

eoy_pop_variance_DIVE <- var(EOY_total_DIVE$total_ducks, na.rm = TRUE)

### AllDucks

EOY_AllDucks <- Dec_22_Ducks %>%
  filter(!(Expert_Opinion %in% "No"))

EOY_total_AllDucks <- EOY_AllDucks %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

EOY_Count_AllDucks <- sum(EOY_total_AllDucks$total_ducks)


eoy_mean_AllDucks <- mean(EOY_total_AllDucks$total_ducks)
eoy_sd_AllDucks <- sd(EOY_total_AllDucks$total_ducks)
eoy_ss_AllDucks <- length(EOY_total_AllDucks$total_ducks)
eoy_SE_AllDucks <- eoy_sd_AllDucks / sqrt(eoy_ss_AllDucks)
eoy_CV_AllDucks <- (eoy_sd_AllDucks/eoy_mean_AllDucks)*100
cat("Standard Error - Dec eoy AllDucks", eoy_SE_AllDucks, "\n")
cat("Coefficient of Variation - Dec eoy AllDucks:", eoy_CV_AllDucks, "%\n")

eoy_pop_variance_AllDucks <- var(EOY_total_AllDucks$total_ducks, na.rm = TRUE)


## No - Expert Opinion

EON_DABB <- Dec_DABB %>%
  filter(!(Expert_Opinion %in% "Yes"))

EON_total_DABB <- EON_DABB %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

EON_Count_DABB <- sum(EON_total_DABB$total_ducks)

### DABB
eon_mean_DABB <- mean(EON_total_DABB$total_ducks)
eon_sd_DABB <- sd(EON_total_DABB$total_ducks)
eon_ss_DABB <- length(EON_total_DABB$Transect)
eon_SE_DABB <- eon_sd_DABB / sqrt(eon_ss_DABB)
eon_CV_DABB <- (eon_sd_DABB/eon_mean_DABB)*100
cat("Standard Error - Dec EON DABB", eon_SE_DABB, "\n")
cat("Coefficient of Variation - Dec EON DABB:", eon_CV_DABB, "%\n")

eon_pop_variance_DABB <- var(EON_total_DABB$total_ducks, na.rm = TRUE)

### DIVE

EON_DIVE <- Dec_DIVE %>%
  filter(!(Expert_Opinion %in% "Yes"))

EON_total_DIVE <- EON_DIVE %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

EON_Count_DIVE <- sum(EON_total_DIVE$total_ducks)


eon_mean_DIVE <- mean(EON_total_DIVE$total_ducks)
eon_sd_DIVE <- sd(EON_total_DIVE$total_ducks)
eon_ss_DIVE <- length(EON_total_DIVE$total_ducks)
eon_SE_DIVE <- eon_sd_DIVE / sqrt(eon_ss_DIVE)
eon_CV_DIVE <- (eon_sd_DIVE/eon_mean_DIVE)*100
cat("Standard Error - Dec eon DIVE", eon_SE_DIVE, "\n")
cat("Coefficient of Variation - Dec  eon DIVE:", eon_CV_DIVE, "%\n")

eon_pop_variance_DIVE <- var(EON_total_DIVE$total_ducks, na.rm = TRUE)

### AllDucks

EON_AllDucks <- Dec_22_Ducks %>%
  filter(!(Expert_Opinion %in% "Yes"))

EON_total_AllDucks <- EON_AllDucks %>%
  group_by(Transect) %>% 
  summarise(total_ducks = sum(Count), .groups = "drop")

EON_Count_AllDucks <- sum(EON_total_AllDucks$total_ducks)


eon_mean_AllDucks <- mean(EON_total_AllDucks$total_ducks)
eon_sd_AllDucks <- sd(EON_total_AllDucks$total_ducks)
eon_ss_AllDucks <- length(EON_total_AllDucks$total_ducks)
eon_SE_AllDucks <- eon_sd_AllDucks / sqrt(eon_ss_AllDucks)
eon_CV_AllDucks <- (eon_sd_AllDucks/eon_mean_AllDucks)*100
cat("Standard Error - Dec eon AllDucks", eon_SE_AllDucks, "\n")
cat("Coefficient of Variation - Dec eon AllDucks:", eon_CV_AllDucks, "%\n")

eon_pop_variance_AllDucks <- var(EON_total_AllDucks$total_ducks, na.rm = TRUE)