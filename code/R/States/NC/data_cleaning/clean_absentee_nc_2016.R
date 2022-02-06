################################################################################
## Clean absentee ballots for 2016
################################################################################
# libraries
library(tidyverse)
library(rstanarm)
library(boot)
################################################################################
# load data
df <- read_csv(file = "data/GE2020/NC/absentee_2016_w_ethnicity.csv")
df_zip_income <- read_csv("data/GE2020/NC/zipcode_median_income.csv")
df_acs <- read_csv("data/acs_econ_13_18.csv")
################################################################################
# wrangle
df <- df %>%
  mutate(pid = ifelse(voter_party_code == "REP", 2, 
                      ifelse(voter_party_code == "UNA", 1, 
                             ifelse(voter_party_code == "DEM", 0, NA))),
         ethn = 
           ifelse(ethnicity == "HISPANIC or LATINO", "hispanic",
           ifelse(race == "WHITE", "white",
           ifelse(race == "BLACK or AFRICAN AMERICAN", "black",
           ifelse(race == "ASIAN", "asian",
           ifelse(race == "INDIAN AMERICAN or ALASKA NATIVE", "native",
           ifelse(race == "NATIVE HAWAIIAN or PACIFIC ISLANDER", "pac", "other"
                                              )))))),
         status = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0),
         ballot_requested = as.Date(ballot_req_dt, "%m/%d/%y"),
         ballot_sent_out = as.Date(ballot_send_dt, "%m/%d/%y"),
         ballot_sent_back = as.Date(ballot_rtn_dt, "%m/%d/%y"),
         ballot_sent_out_sent_back = as.integer(difftime(ballot_sent_back,ballot_sent_out, units = "days"))
         ) 
################################################################################
# Merge in income
df <- merge(df, df_zip_income, by.x = "voter_zip", by.y = "zipcode")
# county income
df_acs <- df_acs %>% 
  filter(grepl("North\\sCarolina", jurisdiction)) %>%
  mutate(jurisdiction = toupper(jurisdiction),
         jurisdiction = gsub("\\sCOUNTY.+", "", jurisdiction))
## Merge in ACS
df <- merge(df, df_acs, by.x = "county_desc", by.y = "jurisdiction") 
################################################################################
