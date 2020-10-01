source("code/States/NC/clean_absentee_nc.R")
df_request_day <- df %>%
  group_by(ballot_requested, ethn) %>%
  summarize(n_ballots = n()) %>%
  ungroup() %>% group_by(ethn) %>%
  mutate(share_ballots = n_ballots/sum(n_ballots)) %>%
  ungroup()

ggplot(data = df_request_day %>%
         filter(ballot_requested >= "2020-06-01"), aes(x = ballot_requested, y = share_ballots, color = ethn)) + 
  geom_line() + 
  ylim(c(0, 0.125))