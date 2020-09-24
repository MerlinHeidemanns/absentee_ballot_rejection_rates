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

duplicates_acs <- df_acs %>% group_by(FIPSCode) %>%
  summarize(count = n()) %>% ungroup() %>%
  filter(count > 1) %>% pull(count) %>% sum()
cat("N of duplicates in the ACS:", duplicates_acs)


cond1 = ifelse(min(merged$rejected, na.rm = TRUE) < 0, TRUE, FALSE)
cond2 = ifelse(max(merged$pr3, na.rm = TRUE) > 1 |
                 min(merged$pr3, na.rm = TRUE) < 0 |
                 max(merged$pr2, na.rm = TRUE) > 1 |
                 min(merged$pr2, na.rm = TRUE) < 0 ,
               TRUE, FALSE)  
merged %>% filter(
  rejected < 0 |
    pr2      > 1 |
    pr2      < 0 |
    pr3      > 1 |
    pr3      < 0
) %>%
  dplyr::select(State, JurisdictionName, rejected, pr2, pr3)