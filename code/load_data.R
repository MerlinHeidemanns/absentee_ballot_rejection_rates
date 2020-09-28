library(tidyverse)
# fun
source("code/functions.R")
# acs data
source("code/clean_acs_data.R")
# eavs data
source("code/clean_eavs_data.R")
# merge
merged <- merge(eavs, df_acs, by = "FIPSCode")
# data inspection
source("code/raw_data_viz.R")
# write
write.csv(merged,"data/eavs_merged_w_acs_2013_2018.csv", na="NA")
# checks
cat("Dimensions merged:", dim(merged), "\n",
    "Dimensions EAVS:", dim(eavs), "\n",
    "Dimensions ACS:", dim(df_acs), "\n")
cat("States merged", length(unique(merged$State)), "\n",
    "States eavs", length(unique(eavs$State)), "\n")
df_acs_coll <- df_acs %>%
  group_by(State) %>%
  summarize(count_acs = n())
df_merged_coll <- merged %>%
  group_by(State) %>%
  summarize(count_merged = n())
df_eavs_coll <- eavs %>%
  group_by(State) %>%
  summarize(count_eavs = n())
df_merged_coll %>%
  full_join(df_eavs_coll, by = "State") %>%
  print(.)
# duplicates
duplicates_acs <- df_acs %>% group_by(FIPSCode) %>%
  summarize(count = n()) %>% ungroup() %>%
  filter(count > 1) %>% pull(count) %>% sum()
cat("N of duplicates in the ACS:", duplicates_acs)
