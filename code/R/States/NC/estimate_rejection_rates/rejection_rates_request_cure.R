################################################################################
## How likely are voters to cure their ballot?
################################################################################
## Libraries
library(tidyverse)
################################################################################
## Load data
source("code/States/NC/data_cleaning/clean_absentee_nc.R")
################################################################################
## Clean the cured data and add income
df_coll_cured <- df_cured %>%
  mutate(income = ifelse(zip_median_income < 45000, "Less than 45k", "Over 45k")) %>%
  group_by(ethn) %>%
  summarize(cure_option = n(),
            cured = sum(cured))
## Model
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(cured, cure_option - cured) ~ (1 | ethn),
             data = df_coll_cured, 
             family = binomial("logit"), prior = wi_prior, cores = 4)
################################################################################
## Analyze output
pred <- posterior_linpred(fit, transform = TRUE, df_coll_cured)
colnames(pred) <- df_coll_cured$ethn
pred <- pred %>% 
  as_tibble() %>%
  pivot_longer(everything(), names_to = "ethn", values_to = "rate") %>%
  group_by(ethn) %>%
  mutate(rate = rate * 100) %>%
  summarize(q25 = quantile(rate, 0.25),
            q75 = quantile(rate, 0.75),
            q10 = quantile(rate, 0.1),
            q90 = quantile(rate, 0.90),
            q50 = quantile(rate, 0.5)
            )
ggplot(data = pred %>% mutate(ethn = factor(ethn, levels = ethn[order(q50)])), aes(x = ethn, y = q50)) + 
  geom_point() + 
  geom_errorbar(aes(x = ethn, ymin = q25, ymax = q75), size = 1, width = 0) +
  geom_errorbar(aes(x = ethn, ymin = q10, ymax = q90), size = 0.5, width = 0) +
  theme_bw() + 
  labs(y = "Probability to cure ballot (in %", 
       caption = "Thin line: 50%
       Thick lin : 80%
       Dot: median", 
       title = "Cure probability by race in NC so far") + 
  theme(axis.title.x = element_blank())
ggsave("plots/States/NC/Cure_probability.jpeg", width = 10, height = 6)
################################################################################
# Interact income with ethnicity
df_coll_cured <- df_cured %>%
  mutate(income = ifelse(zip_median_income < 45000, "Less than 45k", "Over 45k")) %>%
  filter(ethn != "native") %>%
  group_by(ethn, income) %>%
  summarize(cure_change = n(),
            cured = sum(cured))
## Model
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(cured, cure_change - cured) ~ factor(income):factor(ethn) + (1 | ethn) + (1 | income),
             data = df_coll_cured, 
             family = binomial("logit"), prior = wi_prior, cores = 4)
################################################################################
## Analyze output
pred <- posterior_linpred(fit, transform = TRUE)
colnames(pred) <- paste(df_coll_cured$ethn, df_coll_cured$income, sep = "_")
pred <- as.data.frame(pred)
pred <- pred %>% 
  pivot_longer(everything(), names_to = c("ethn", "income"), names_sep = "_", values_to = "prob")
  
ggplot(pred %>% mutate(income = factor(income, levels = c("Less than 45k", "Over 45k"))), aes(y = income, color = ethn)) +
  stat_pointinterval(aes(x = prob), .width = c(.66, .95), position = position_dodge()) + 
  coord_flip()
ggsave("plots/States/NC/Cure_probability_by_income_race.jpeg")
################################################################################
## Interact party code with ethnicty
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
################################################################################
## Analyze output
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
################################################################################