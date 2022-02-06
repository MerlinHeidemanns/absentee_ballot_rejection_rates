################################################################################
## Load libraries
library(tidyverse)
################################################################################
# Load functions
source("code/R/functions/functions_national.R")
################################################################################
## Clean and load data
# acs data
source("code/R/National/clean_acs_data.R")
# eavs data
source("code/R/National/clean_eavs_data.R")
################################################################################
# merge EAVS and ACS
merged <- merge(eavs, df_acs, by = "FIPSCode")
# data inspection
source("code/R/National/raw_data_viz.R")
# write
write.csv(merged,"data/eavs_merged_w_acs_2013_2018.csv", na="NA")
################################################################################
## Data checks
# Dimensions
cat("Dimensions merged:", dim(merged), "\n",
    "Dimensions EAVS:", dim(eavs), "\n",
    "Dimensions ACS:", dim(df_acs), "\n")
cat("States merged", length(unique(merged$State)), "\n",
    "States eavs", length(unique(eavs$State)), "\n")
df_merged_coll <- merged %>%
  group_by(State) %>%
  summarize(count_merged = n())
df_eavs_coll <- eavs %>%
  group_by(State) %>%
  summarize(count_eavs = n())
df_merged_coll %>%
  full_join(df_eavs_coll, by = "State") %>%
  print(.)
# Duplicates
duplicates_acs <- df_acs %>% group_by(FIPSCode) %>%
  summarize(count = n()) %>% ungroup() %>%
  filter(count > 1) %>% pull(count) %>% sum()
cat("N of duplicates in the ACS:", duplicates_acs)
