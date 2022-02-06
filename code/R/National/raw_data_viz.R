################################################################################
## Visualization of the raw data
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
state_rep_voteshare <- read.csv("data/state_rep_voteshare.csv")
################################################################################
## Merge the data and subset the states
merged_viz <- merged %>% 
  left_join(state_rep_voteshare, by = "State")
rep_voteshare <- state_rep_voteshare %>% 
  arrange(rep_vote_share) %>%
  filter(State %in% merged_viz$State)
################################################################################
## Plot
ggplot(data = merged_viz %>% 
         mutate(State = factor(State, levels = rep_voteshare %>% pull(State))) %>%
         select(State, pr1, pr2, pr3) %>% 
         group_by(State) %>% 
           summarize_all(list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE))) %>% 
         pivot_longer(cols = c(-State),names_to = c("kind", ".value"),
                      names_pattern = "(.+)_(.+)")
         ) + 
  geom_point(aes(x = State, y = mean), size = 0.8) +
  geom_errorbar(aes(x = State, ymax = mean + 2/3 * sd, ymin = mean - 2/3 * sd), width = 0) +
  coord_flip() + 
  ylim(c(0,1)) + 
  labs(x = "Share") + 
  facet_grid(~kind, scales = "free") + 
  theme_bw()
ggsave(filename = "plots/raw_viz/rates_mean_by_state.jpeg")
################################################################################
rm(list(merged_viz, rep_voteshare))
################################################################################