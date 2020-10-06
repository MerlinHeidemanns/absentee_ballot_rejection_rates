# libraries
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(boot)
library(ggmap)
# 2020
df_2020 <- read_csv(file = "data/GE2020/NC/absentee_2020_10_05.csv")
# wrangle
df_2020 <- df_2020 %>%
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
         reg_num = as.integer(voter_reg_num)) %>%
  filter(kind == "CIV")
# 2018
df_2018 <- read_csv(file = "data/GE2020/NC/absentee_2018.csv")
df_2018 <- df_2018 %>%
  filter(ballot_req_type == "MAIL",
         ballot_rtn_status != " RETURNED UNDELIVERABLE") %>%
  mutate(ethn = 
           ifelse(ethnicity == "HISPANIC or LATINO", "hispanic",
           ifelse(race == "WHITE", "white",
           ifelse(race == "BLACK or AFRICAN AMERICAN", "black",
           ifelse(race == "ASIAN", "asian",
           ifelse(race == "INDIAN AMERICAN or ALASKA NATIVE", "native",
           ifelse(race == "NATIVE HAWAIIAN or PACIFIC ISLANDER", "pac", "other"
                                              )))))),
         status_2018 = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0),
         reg_num = as.integer(voter_reg_num),
         voted_by_mail_2018 = 1) %>%
  select(ncid, voted_by_mail_2018, status_2018)
# 2016
df_2016 <- read_csv(file = "data/GE2020/NC/absentee_2016.csv")
df_2016 <- df_2016 %>%
  filter(ballot_req_type == "MAIL",
         ballot_rtn_status != " RETURNED UNDELIVERABLE") %>%
  mutate(status_2016 = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0),
         reg_num = as.integer(voter_reg_num),
         voted_by_mail_2016 = 1) %>%
  select(ncid, voted_by_mail_2016, status_2016)
# # 2014
# df_2014 <- read_csv(file = "data/GE2020/NC/absentee_2014.csv")
# df_2014 <- df_2014 %>%
#   filter(ballot_req_type == "MAIL",
#          ballot_rtn_status != " RETURNED UNDELIVERABLE") %>%
#   mutate(status_2014 = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0),
#          reg_num = as.integer(voter_reg_num),
#          voted_by_mail_2014 = 1) %>%
#   select(ncid, voted_by_mail_2014, status_2014)
# # 2012
# df_2012 <- read_csv(file = "data/GE2020/NC/absentee_2012.csv")
# df_2014 <- df_2014 %>%
#   filter(ballot_req_type == "MAIL",
#          ballot_rtn_status != " RETURNED UNDELIVERABLE") %>%
#   mutate(status_2014 = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0),
#          reg_num = as.integer(voter_reg_num),
#          voted_by_mail_2014 = 1) %>%
#   select(ncid, voted_by_mail_2014, status_2014)

# merge
df_2020 <- merge(df_2020, df_2018, by = "ncid", all.x = TRUE)
df_2020 <- merge(df_2020, df_2016, by = "ncid", all.x = TRUE)
df_2020 <- df_2020 %>% 
  mutate(voted_by_mail_2016 = ifelse(is.na(voted_by_mail_2016), 0, voted_by_mail_2016),
         voted_by_mail_2018 = ifelse(is.na(voted_by_mail_2018), 0, voted_by_mail_2018),
         voted_by_mail_last_four_years = ifelse(voted_by_mail_2016 == 1 | voted_by_mail_2018 == 1, 1, 0),
         )
write_csv(df_2020, path = "data/GE2020/NC/absentee_2020_10_05_w_2016_18.csv")


