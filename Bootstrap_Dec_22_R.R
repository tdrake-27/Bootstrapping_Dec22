library(boot)
library(bootstrap)
library(dplyr)
library(readxl)
library(rlang)
library(tidyverse)
library(mosaic)


Dec_22_R <- read_excel("Dec_22_R.xls") ### Pull in full dataset from flights

Remove_geese_coot <- c("AMCO","CAGO","DEADDU","DEADGO","DKGO","GOLD","GWFG","LESG","LSGO","LIGO","SACR") ### Define undesired species (Geese, coots, etc)

DABB <- c("ABDU","AMWI","AGWT","BBWD","BWTE","CITE","DABB","FUWD","GADW","MALL","NOPI","NSHO","TEAL","WODU") ### Define Dabblers

DIVE <- c("BAGO","BUFF","COGO","CANV","DIVE","GRSC","HOME","LESC","MERG","RBME","RNDU","REDH","SCAU") ### Define Divers

Dec_22_Ducks <- Dec_22_R %>%
  filter(!(Species %in% Remove_geese_coot)) ### Remove undesired species

Master_df <- read_excel("11_03_2022_Ten Percent Chosen.xlsx")  ### Bring in master_df to recall transect lines

pre_boot <- Dec_22_Ducks ### Rename the ducks df to be pre_boot for the df to use for bootstrapping ducks only


# Define the statistic function
statistic_function <- function(data, indices) {
  resampled_data <- data[indices, ]
  
  # Split the data into categories
  Dec_DIVE <- resampled_data %>%
    filter(!Species %in% DABB)
  
  Dec_DABB <- resampled_data %>%
    filter(!Species %in% DIVE)
  
  # Calculate total ducks per transect for each category
  dec_total_DABB <- Dec_DABB %>%
    group_by(Transect) %>% 
    summarise(total_ducks = sum(Count), .groups = "drop")
  
  dec_total_DIVE <- Dec_DIVE %>%
    group_by(Transect) %>% 
    summarise(total_ducks = sum(Count), .groups = "drop")
  
  dec_total_AllDucks <- resampled_data %>%
    group_by(Transect) %>% 
    summarise(total_ducks = sum(Count), .groups = "drop")
  
  # Combine results into a list or a vector
  c(mean(dec_total_DABB$total_ducks), 
    mean(dec_total_DIVE$total_ducks), 
    mean(dec_total_AllDucks$total_ducks))
}

# Perform bootstrapping
set.seed(123) # For reproducibility
bootstrap_results <- boot(data = pre_boot, statistic = statistic_function, R = 10000)

# Extract bootstrapped statistics for each category
bootstrapped_DABB <- bootstrap_results$t[, 1]
bootstrapped_DIVE <- bootstrap_results$t[, 2]
bootstrapped_AllDucks <- bootstrap_results$t[, 3]

# Summary statistics for each category
summary(bootstrapped_DABB)
summary(bootstrapped_DIVE)
summary(bootstrapped_AllDucks)

# Plot the results for each category
hist(bootstrapped_DABB, main = "Bootstrapped Means of DABB", xlab = "Mean of Total Ducks (DABB)")
hist(bootstrapped_DIVE, main = "Bootstrapped Means of DIVE", xlab = "Mean of Total Ducks (DIVE)")
hist(bootstrapped_AllDucks, main = "Bootstrapped Means of All Ducks", xlab = "Mean of Total Ducks (All)")


# Function to calculate statistics
calculate_statistics <- function(data) {
  mean_val <- mean(data)
  sd_val <- sd(data)
  ss_val <- length(data)
  se_val <- sd_val / sqrt(ss_val)
  cv_val <- (sd_val / mean_val) * 100
  var_val <- var(data, na.rm = TRUE)
  
  list(
    mean = mean_val,
    sd = sd_val,
    se = se_val,
    cv = cv_val,
    variance = var_val
  )
}

# Bootstrapped calculation function
bootstrap_and_calculate <- function(category_data) {
  if (length(category_data) == 0) {
    return(NULL)
  }
  # Perform bootstrapping
  set.seed(123) # For reproducibility
  bootstrap_results <- boot(data = category_data, statistic = function(data, indices) mean(data[indices]), R = 10000)
  
  # Extract bootstrapped statistics
  bootstrapped_data <- bootstrap_results$t
  
  # Calculate statistics
  calculate_statistics(bootstrapped_data)
}

# Define categories and their filters for Watershed and Expert Opinion Areas
watershed_categories <- list(
  BT_DABB = pre_boot %>% filter(!(Stratum %in% c("LO", "LR")), !Species %in% DIVE),
  LO_DABB = pre_boot %>% filter(!(Stratum %in% c("BT", "LR")), !Species %in% DIVE),
  LR_DABB = pre_boot %>% filter(!(Stratum %in% c("BT", "LO")), !Species %in% DIVE),
  BT_DIVE = pre_boot %>% filter(!(Stratum %in% c("LO", "LR")), !Species %in% DABB),
  LO_DIVE = pre_boot %>% filter(!(Stratum %in% c("BT", "LR")), !Species %in% DABB),
  LR_DIVE = pre_boot %>% filter(!(Stratum %in% c("BT", "LO")), !Species %in% DABB),
  BT_AllDucks = pre_boot %>% filter(!(Stratum %in% c("LO", "LR"))),
  LO_AllDucks = pre_boot %>% filter(!(Stratum %in% c("BT", "LR"))),
  LR_AllDucks = pre_boot %>% filter(!(Stratum %in% c("BT", "LO")))
)

expert_opinion_categories <- list(
  EOY_DABB = pre_boot %>% filter(!(Expert_Opinion %in% "No"), !Species %in% DIVE),
  EOY_DIVE = pre_boot %>% filter(!(Expert_Opinion %in% "No"), !Species %in% DABB),
  EOY_AllDucks = pre_boot %>% filter(!(Expert_Opinion %in% "No")),
  EON_DABB = pre_boot %>% filter(!(Expert_Opinion %in% "Yes"), !Species %in% DIVE),
  EON_DIVE = pre_boot %>% filter(!(Expert_Opinion %in% "Yes"), !Species %in% DABB),
  EON_AllDucks = pre_boot %>% filter(!(Expert_Opinion %in% "Yes"))
)

# Define categories and their filters for R10%
R10_categories <- list(
  DABB = pre_boot %>% filter(!Species %in% DIVE),
  DIVE = pre_boot %>% filter(!Species %in% DABB),
  AllDucks = pre_boot
)

# Apply the calculations for each category and print results

# Watershed Results
watershed_results <- list()
cat("Watershed Results:\n\n")
for (name in names(watershed_categories)) {
  category_data <- watershed_categories[[name]] %>%
    group_by(Transect) %>%
    summarise(total_ducks = sum(Count), .groups = "drop") %>%
    pull(total_ducks)
  
  if (length(category_data) == 0) {
    cat(paste(name, "Statistics:\n"))
    cat("No data available\n\n")
    next
  }
  
  # Calculate statistics using bootstrapping
  stats <- bootstrap_and_calculate(category_data)
  watershed_results[[name]] <- stats
  
  # Print results
  cat(paste(name, "Statistics:\n"))
  cat("Mean:", stats$mean, "\n")
  cat("SD:", stats$sd, "\n")
  cat("SE:", stats$se, "\n")
  cat("CV:", stats$cv, "%\n")
  cat("Variance:", stats$variance, "\n\n")
}

# Expert Opinion Results
expert_opinion_results <- list()
cat("Expert Opinion Results:\n\n")
for (name in names(expert_opinion_categories)) {
  category_data <- expert_opinion_categories[[name]] %>%
    group_by(Transect) %>%
    summarise(total_ducks = sum(Count), .groups = "drop") %>%
    pull(total_ducks)
  
  if (length(category_data) == 0) {
    cat(paste(name, "Statistics:\n"))
    cat("No data available\n\n")
    next
  }
  
  # Calculate statistics using bootstrapping
  stats <- bootstrap_and_calculate(category_data)
  expert_opinion_results[[name]] <- stats
  
  # Print results
  cat(paste(name, "Statistics:\n"))
  cat("Mean:", stats$mean, "\n")
  cat("SD:", stats$sd, "\n")
  cat("SE:", stats$se, "\n")
  cat("CV:", stats$cv, "%\n")
  cat("Variance:", stats$variance, "\n\n")
}

# R10% Results
R10_results <- list()
cat("R10% Results:\n\n")
for (name in names(R10_categories)) {
  category_data <- R10_categories[[name]] %>%
    group_by(Transect) %>%
    summarise(total_ducks = sum(Count * 0.10), .groups = "drop") %>%
    pull(total_ducks)
  
  if (length(category_data) == 0) {
    cat(paste("R10%", name, "Statistics:\n"))
    cat("No data available\n\n")
    next
  }
  
  # Calculate statistics using bootstrapping
  stats <- bootstrap_and_calculate(category_data)
  R10_results[[name]] <- stats
  
  # Print results
  cat(paste("R10%", name, "Statistics:\n"))
  cat("Mean:", stats$mean, "\n")
  cat("SD:", stats$sd, "\n")
  cat("SE:", stats$se, "\n")
  cat("CV:", stats$cv, "%\n")
  cat("Variance:", stats$variance, "\n\n")
}
