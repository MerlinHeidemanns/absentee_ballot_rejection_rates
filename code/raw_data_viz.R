# raw data viz
library(tidyverse)
merged_viz <- merged %>% left_join(read.csv("data/state_rep_voteshare.csv"), by = "State")
rep_voteshare <- read.csv("data/state_rep_voteshare.csv") %>% 
  arrange(rep_vote_share) %>%
  filter(State %in% merged_viz$State)

ggplot(data = merged_viz %>% 
         mutate(State = factor(State, levels = rep_voteshare %>% pull(State))) %>%
         select(State, pr1, pr2, pr3) %>% 
         group_by(State) %>% 
           summarize_all(list(~ mean(., na.rm = TRUE), ~ sd(., na.rm = TRUE))) %>% 
         pivot_longer(cols = c(-State),names_to = c("kind", ".value"),
                      names_pattern = "(.+)_(.+)")
         ) + 
  geom_point(aes(x = State, y = mean)) +
  coord_flip() + 
  facet_wrap(~kind, scales = "free") 