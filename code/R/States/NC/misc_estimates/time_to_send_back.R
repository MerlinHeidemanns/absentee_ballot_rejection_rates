################################################################################
## Time to send back ballot by ethnic group
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
source("code/R/States/NC/data_cleaning/clean_absentee_nc_2020.R")
################################################################################
## Compile data and plot
df %>% 
  group_by(ethn, age) %>%
  summarize(mean_time = mean(ballot_sent_out_sent_back),
            sd_time   = sd(ballot_sent_out_sent_back)) %>%
  ggplot(., aes(x = age, y = mean_time)) +
    geom_point() +
    geom_errorbar(aes(x = age, ymin = mean_time - 2/3 * sd_time,
                      ymax = mean_time - 2/3 * sd_time), width = 0) +
  theme_bw() + 
  facet_wrap(.~ethn)