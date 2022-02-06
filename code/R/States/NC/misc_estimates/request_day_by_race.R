################################################################################
## Plot the share of ballots that has been requested by a particular day by race
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
source("code/R/States/NC/data_cleaning/clean_absentee_nc_2020.R")
################################################################################
## Mangle data
df_request_day <- df %>%
  group_by(ballot_requested, ethn) %>%
  summarize(n_ballots = n()) %>%
  ungroup() %>% group_by(ethn) %>%
  mutate(share_ballots = n_ballots/sum(n_ballots)) %>%
  ungroup()
## Plot
ggplot(data = df_request_day %>%
         filter(ballot_requested >= "2020-06-01"), aes(x = ballot_requested, y = share_ballots, color = ethn)) + 
  geom_line() + 
  ylim(c(0, 0.125))