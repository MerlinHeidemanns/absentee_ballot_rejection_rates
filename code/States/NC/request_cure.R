source("code/States/NC/clean_absentee_nc.R")
df_coll_cured <- df_cured %>%
  mutate(income = ifelse(zip_median_income < 45000, "Less than 45k", "Over 45k")) %>%
  filter(ethn != "native") %>%
  group_by(ethn, income) %>%
  summarize(cure_change = n(),
            cured = sum(cured))
# model
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(cured, cure_change - cured) ~ factor(income):factor(ethn) + (1 | ethn) + (1 | income),
             data = df_coll_cured, 
             family = binomial("logit"), prior = wi_prior, cores = 4)
pred <- posterior_linpred(fit, transform = TRUE)
colnames(pred) <- paste(df_coll_cured$ethn, df_coll_cured$income, sep = "_")
pred <- as.data.frame(pred)
pred <- pred %>% 
  pivot_longer(everything(), names_to = c("ethn", "income"), names_sep = "_", values_to = "prob")
  
ggplot(pred %>% mutate(income = factor(income, levels = c("Less than 45k", "Over 45k"))), aes(y = income, color = ethn)) +
  stat_pointinterval(aes(x = prob), .width = c(.66, .95), position = position_dodge()) + 
  coord_flip()
ggsave("plots/States/NC/Cure_probability_by_income_race.jpeg")
# by pid -----
df_coll_cured <- df_cured %>%
  filter(ethn != "native", voter_party_code %in% c("REP", "UNA", "DEM")) %>%
  group_by(ethn, voter_party_code) %>%
  summarize(cure_change = n(),
            cured = sum(cured))
# model
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(cured, cure_change - cured) ~ factor(voter_party_code):factor(ethn) + (1 | ethn) + (1 | voter_party_code),
             data = df_coll_cured, 
             family = binomial("logit"), prior = wi_prior, cores = 4)
pred <- posterior_linpred(fit, transform = TRUE)
colnames(pred) <- paste(df_coll_cured$ethn, df_coll_cured$voter_party_code, sep = "_")
pred <- as.data.frame(pred)
pred <- pred %>% 
  pivot_longer(everything(), names_to = c("ethn", "voter_party_code"), names_sep = "_", values_to = "prob")

ggplot(pred %>% mutate(voter_party_code = factor(voter_party_code, levels = c("DEM", "UNA", "REP"))), aes(y = voter_party_code, color = ethn)) +
  stat_pointinterval(aes(x = prob), .width = c(.50, .80), position = position_dodge()) + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.title.x = element_blank()) +
  labs(color = "Race", x = "Probability to cure ballot")
ggsave("plots/States/NC/Cure_probability_by_pid_race.jpeg")








  