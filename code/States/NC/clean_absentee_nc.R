# libraries
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(boot)
library(ggmap)
# data
df <- read_csv(file = "data/GE2020/NC/absentee_2020_09_25.csv")
# wrangle
df <- df %>%
  filter(ballot_rtn_status != "RETURNED UNDELIVERABLE",
         ballot_rtn_status != "PENDING") %>%
  mutate(ethn = 
           ifelse(ethnicity == "HISPANIC or LATINO", "hispanic",
           ifelse(race == "WHITE", "white",
           ifelse(race == "BLACK or AFRICAN AMERICAN", "black",
           ifelse(race == "ASIAN", "asian",
           ifelse(race == "INDIAN AMERICAN or ALASKA NATIVE", "native",
           ifelse(race == "NATIVE HAWAIIAN or PACIFIC ISLANDER", "pac", 
                  "other"
                  )))))),
         kind = gsub("[^A-Z]", "", application_num),
         status = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0),
         ballot_requested = as.Date(ballot_req_dt, "%m/%d/%y"))
df_rejected <- df %>% 
  filter(ballot_rtn_status != "ACCEPTED") %>%
  mutate(request_cure = ifelse(ballot_rtn_status == "ACCEPTED - CURED" | 
                               ballot_rtn_status == "PENDING CURE", 1, 0))

df_acs <- read_csv("data/acs_econ_13_18.csv") %>% 
  filter(grepl("North\\sCarolina", jurisdiction)) %>%
  mutate(jurisdiction = toupper(jurisdiction),
         jurisdiction = gsub("\\sCOUNTY.+", "", jurisdiction))
df <- merge(df, df_acs, by.x = "county_desc", by.y = "jurisdiction") 





