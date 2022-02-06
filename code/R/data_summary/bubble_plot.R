# bubble plot

df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state_allVBM_requested.Rds")
df_sim <- df_sim %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup()
df_shares <- df_sim %>% 
  transmute(
    State = State,
    sim = sim,
    voters_sum = voters_white_sum + voters_black_sum + voters_hispanic_sum + voters_asian_sum + voters_other_sum,
    share_voters_white = voters_white_sum / voters_sum,
    share_voters_black = voters_black_sum / voters_sum,
    share_voters_hispanic = voters_hispanic_sum / voters_sum,
    share_voters_asian = voters_asian_sum / voters_sum,
    share_voters_other = voters_other_sum / voters_sum,
    share_requested_white = n_requested_white_sum / n_requested_sum,
    share_requested_black = n_requested_black_sum / n_requested_sum,
    share_requested_hispanic = n_requested_hispanic_sum / n_requested_sum,
    share_requested_asian = n_requested_asian_sum / n_requested_sum,
    share_requested_other = n_requested_other_sum / n_requested_sum,
    share_submitted_white = n_submitted_white_sum / n_submitted_sum,
    share_submitted_black = n_submitted_black_sum / n_submitted_sum,
    share_submitted_hispanic = n_submitted_hispanic_sum / n_submitted_sum,
    share_submitted_asian = n_submitted_asian_sum / n_submitted_sum,
    share_submitted_other = n_submitted_other_sum / n_submitted_sum,
    share_rejected_white = n_rejected_white_sum / n_rejected_sum,
    share_rejected_black = n_rejected_black_sum / n_rejected_sum,
    share_rejected_hispanic = n_rejected_hispanic_sum / n_rejected_sum,
    share_rejected_asian = n_rejected_asian_sum / n_rejected_sum,
    share_rejected_other = n_rejected_other_sum / n_rejected_sum
  ) %>%   
  group_by(State, ) %>% 
  dplyr::select(-sim) %>%
  summarize_all(list(~ mean(.), ~ sd(.))) %>%
  ungroup()
# hispanic
ggplot(data = df_shares, aes(x = share_submitted_hispanic_mean, y = share_rejected_hispanic_mean)) + 
  geom_point(aes(size = voters_sum_mean), fill = "blue", color = "black", alpha = 0.3) +
  geom_text(aes(label = State), vjust = -0.25, hjust = -0.25, 
            data = df_shares %>% 
              filter(abs(share_rejected_hispanic_mean - share_submitted_hispanic_mean) > 0.075 |
                       (share_rejected_hispanic_mean - share_submitted_hispanic_mean) < 0)) +
  lims(x = c(0, 0.5), y = c(0, 0.5)) + 
  geom_abline() + 
  theme_bw()
# black
ggplot(data = df_shares, aes(x = share_submitted_black_mean, y = share_rejected_black_mean)) + 
  geom_point(aes(size = voters_sum_mean), fill = "blue", color = "black", alpha = 0.3) +
  geom_text(aes(label = State), vjust = 0, hjust = -0.25, 
            data = df_shares %>% 
              filter(voters_sum_mean > mean(voters_sum_mean))) +
  lims(x = c(0, 0.5), y = c(0, 0.5)) + 
  geom_abline() + 
  scale_size_continuous(labels = comma) +
  theme_bw()


