################################################################################
## Absentee ballot rejection rate over age
################################################################################
## Includes all over age
#' just age
#' white v non-white
#' male v female
#' male v female by race
#' male v female by pid
#' by race and income
#' white v black by income
#' income and pid
#' income and race
#' pid race income
#' male v race v income
#' pid race income
################################################################################
## Load libraries
library(tidyverse)
library(rstan)
################################################################################
## Load data
source("code/States/NC/data_cleaning/clean_absentee_nc_2020.R")
################################################################################
## Load functions
source("code/States/NC/functions.R")
################################################################################
## Fixed text
caption_text = "Line: Median
                  Inner band: 50%
                  Outer band: 80%"
y_text = "Rejection probability (in %)"
################################################################################
# rates by age ----------------------------------------------------------------
## * Wrangle data
df_collapse <- df %>%
  mutate(outcome = abs(status - 1)) %>%
  group_by(age) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>%
  mutate(age = factor(age, levels = sort(unique(age))))
# * Model
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(rejected, ballots) ~ (1 | age), data = df_collapse, 
             family = binomial("logit"), prior = wi_prior, cores = 4)
# * Analyze output
draws <- fit %>%
  spread_draws(b[t,g], `(Intercept)`) %>%
  dplyr::select(g, b, `(Intercept)`) %>%
  group_by(g) %>%
  mutate(b = inv.logit(b + `(Intercept)`)) %>%
  summarize(mean = mean(b),
            sd = sd(b)) %>%
  ungroup() %>%
  mutate(g = gsub("[^0-9]", "", g),
         g = as.integer(g))
# * Plot
ggplot(data = draws) +
  geom_point(aes(x = g, y = mean)) + 
  geom_errorbar(aes(x = g, ymax = mean + 2/3 * sd, ymin = mean - 2/3 * sd), width = 0) +
  scale_x_continuous(name="Age", limits=c(18, 106)) + 
  labs(caption = "Intervals are 50% certainty intervals.", 
       y = y_text, 
       title = paste0("Absentee ballots in NC. Date: ", Sys.Date())) + 
  theme_bw()
################################################################################
## white v non-white
# * Wrangle data
df_collapse <- df %>%
  filter(age <= 95,
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  group_by(age, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age))))
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(white == 0) %>% pull(ballots),
                   df_collapse %>% filter(white == 1) %>% pull(ballots))
rejected <- cbind(df_collapse %>% filter(white == 0) %>% pull(rejected),
                  df_collapse %>% filter(white == 1) %>% pull(rejected))
data <- list(
  N = as.integer(nrow(rejected)),
  G = 2,
  submitted = submitted,
  rejected = rejected,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 6000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.90)))
df_plot <- data.frame(age = rep(seq(18, 95),2),
                      ethn = c(rep("Black", dim(medians)[1]), rep("white", dim(medians)[1])),
                      median = c(medians[,1], medians[,2]),
                      q25 = c(q25[,1], q25[,2]),
                      q75 = c(q75[,1], q75[,2]),
                      q10 = c(q10[,1], q10[,2]),
                      q90 = c(q90[,1], q90[,2]))
# * Plot
ggplot(data = df_plot, 
       aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = paste("Estimated rejection rates by race, and age in NC for", year),
       color = "Race", 
       fill = "Race")
ggsave(paste0("plots/States/NC/Rejected_rates_by_age_ethnicity_NC_", year ,".jpeg"), width = 9, height = 5)

################################################################################
# male v female -------------
df_collapse <- df %>%
  filter(age <= 95,
         gender != "U") %>%
  mutate(outcome = abs(status - 1),
         male = ifelse(gender == "M",1,0)) %>%
  group_by(age, male) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age))))
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(male == 0) %>% pull(ballots),
                   df_collapse %>% filter(male == 1) %>% pull(ballots))
rejected <- cbind(df_collapse %>% filter(male == 0) %>% pull(rejected),
                  df_collapse %>% filter(male == 1) %>% pull(rejected))
data <- list(
  N = as.integer(nrow(rejected)),
  G = 2, 
  submitted = submitted,
  rejected = rejected, 
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# * Plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
means <- apply(theta, MARGIN = c(2,3), mean)
sds   <- apply(theta, MARGIN = c(2,3), sd)
df_plot <- data.frame(age = rep(seq(18, 95),2),
                      ethn = c(rep("female", 96 - 18), rep("male", 96 - 18)),
                      mean = c(means[,1], means[,2]),
                      sd = c(sds[,1], sds[,2]))
ggplot(data = df_plot, aes(x = age, y = mean, color = ethn)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymax = mean + 2/3 * sd, ymin =  mean - 2/3 * sd, color = ethn), width = 0) + 
  theme_bw() +
  labs(x = "Age", 
       y = y_text, 
       caption = "50% certainty intervals", 
       title = "Estimated rejection rates by ethnicity and age in NC so far",
       color = "Gender")
ggsave("plots/States/NC/Rejected_rates_by_age_gender_NC.jpeg", width = 9, height = 5)

################################################################################
# male v female by race -------
df_collapse <- df %>%
  filter(age <= 95,
         gender != "U",
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         male = ifelse(gender == "M",1,0),
         white = ifelse(ethn == "white",1,0)) %>%
  group_by(age, male, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(male == 0, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(male == 1, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(male == 0, white == 1) %>% pull(ballots),
                   df_collapse %>% filter(male == 1, white == 1) %>% pull(ballots)
)
rejected <- cbind(df_collapse %>% filter(male == 0, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(male == 1, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(male == 0, white == 1) %>% pull(rejected),
                  df_collapse %>% filter(male == 1, white == 1) %>% pull(rejected)
)
data <- list(
  N = as.integer(nrow(rejected)),
  G = 4, 
  submitted = submitted,
  rejected = rejected,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.90)))
df_plot <- data.frame(age = rep(seq(18, 95),4),
                      gender = rep(c(rep("female", dim(means)[1]), rep("male", dim(means)[1])),2),
                      ethn = c(rep("Black", dim(means)[1] * 2), rep("white", dim(means)[1] * 2)),
                      median = c(medians[,1], medians[,2], medians[,3], medians[,4]),
                      q25 = c(q25[,1], q25[,2],q25[,3], q25[,4]),
                      q75 = c(q75[,1], q75[,2],q75[,3], q75[,4]),
                      q10 = c(q10[,1], q10[,2],q10[,3], q10[,4]),
                      q90 = c(q90[,1], q90[,2],q90[,3], q90[,4]))
# * Plot
ggplot(data = df_plot, 
       aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by income, race, and age in NC so far",
       color = "Race", 
       fill = "Race") +
  facet_wrap(~gender)
ggsave("plots/States/NC/Rejected_rates_by_age_gender_race_NC.jpeg", width = 9, height = 5)

################################################################################
# male v female by pid ----------
# * Wrangle data
df_collapse <- df %>%
  filter(age <= 95,
         voter_party_code %in% c("DEM", "REP", "UNA"),
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0),
         pid = ifelse(voter_party_code == "REP",2,
                      ifelse(voter_party_code == "UNA",1,
                             0))) %>%
  group_by(age, pid, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(pid == 0, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 1, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 2, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 0, white == 1) %>% pull(ballots),
                   df_collapse %>% filter(pid == 1, white == 1) %>% pull(ballots),
                   df_collapse %>% filter(pid == 2, white == 1) %>% pull(ballots)                   
)
rejected <- cbind(df_collapse %>% filter(pid == 0, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 1, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 2, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 0, white == 1) %>% pull(rejected),
                  df_collapse %>% filter(pid == 1, white == 1) %>% pull(rejected),
                  df_collapse %>% filter(pid == 2, white == 1) %>% pull(rejected)                   
)
data <- list(
  N = as.integer(nrow(rejected)),
  G = 6, 
  submitted = submitted,
  rejected = rejected,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 8000)
# Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.9)))
df_plot <- data.frame(age = rep(seq(18, 95),6),
                      ethn = c(rep("Black", dim(medians)[1] * 3), rep("white", dim(medians)[1] * 3)),
                      pid =  rep(c(rep("Democratic", dim(medians)[1]), rep("Not aligned", dim(medians)[1]), rep("Republican", dim(medians)[1])), 2),
                      median = c(medians[,1], medians[,2], medians[,3], medians[,4], medians[,5], medians[,6]),
                      q25 = c(q25[,1], q25[,2],q25[,3], q25[,4], q25[,5], q25[,6]),
                      q75 = c(q75[,1], q75[,2],q75[,3], q75[,4], q75[,5], q75[,6]),
                      q10 = c(q10[,1], q10[,2],q10[,3], q10[,4], q10[,5], q10[,6]),
                      q90 = c(q90[,1], q90[,2],q90[,3], q90[,4], q90[,5], q90[,6]))
# * Plot
ggplot(data = df_plot, aes(x = age, y = median, color = ethn)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymin = q25, ymax =  q75, color = ethn), width = 0, size = 0.75) + 
  geom_errorbar(aes(x = age, ymin = q10, ymax = q90, color = ethn), width = 0, size = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = "Thick line: 50%
                  Thin line: 80%", 
       title = "Estimated rejection rates by ethnicity, party id, and age in NC so far",
       color = "Race") +
  facet_wrap(~pid)
ggsave("plots/States/NC/Rejected_rates_by_age_pid_race_NC.jpeg", width = 9, height = 5)

################################################################################
# by race and income------
med_income = median(unique(df$median_household_income))
# * Wrangle data
df_collapse <- df %>%
  filter(age <= 95) %>%
  mutate(outcome = abs(status - 1),
         income = ifelse(median_household_income > med_income, 1, 0),
         white = ifelse(ethn == "white",1,0)) %>%
  group_by(age, income, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(income == 0, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(income == 1, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(income == 0, white == 1) %>% pull(ballots),
                   df_collapse %>% filter(income == 1, white == 1) %>% pull(ballots)
)
rejected <- cbind(df_collapse %>% filter(income == 0, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(income == 1, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(income == 0, white == 1) %>% pull(rejected),
                  df_collapse %>% filter(income == 1, white == 1) %>% pull(rejected)
)
data <- list(
  N = as.integer(nrow(rejected)),
  G = 4, 
  submitted = submitted,
  rejected = rejected,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.9)))
df_plot <- data.frame(age = rep(seq(18, 95),4),
                      ethn = c(rep("non-white", dim(medians)[1] * 2), rep("white", dim(medians)[1] * 2)),
                      income =  rep(c(rep("Below median income", dim(medians)[1]), rep("Above median income", dim(medians)[1])), 2),
                      median = c(medians[,1], medians[,2], medians[,3], medians[,4]),
                      q25 = c(q25[,1], q25[,2],q25[,3], q25[,4]),
                      q75 = c(q75[,1], q75[,2],q75[,3], q75[,4]),
                      q10 = c(q10[,1], q10[,2],q10[,3], q10[,4]),
                      q90 = c(q90[,1], q90[,2],q90[,3], q90[,4]))
ggplot(data = df_plot %>% 
         mutate(income = factor(income, levels = c("Below median income", "Above median income"))), aes(x = age, y = median, color = ethn)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymin = q25, ymax =  q75, color = ethn), width = 0, size = 0.75) + 
  geom_errorbar(aes(x = age, ymin = q10, ymax = q90, color = ethn), width = 0, size = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by income, race, and age in NC so far",
       color = "Race") +
  facet_wrap(~income)
ggsave("plots/States/NC/Rejected_rates_by_age_income_race_NC.jpeg", width = 9, height = 5)


################################################################################
# white black, income------
med_income = median(unique(df$median_household_income))
# * Wrangle data
df_collapse <- df %>%
  filter(age <= 95,
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         income = ifelse(median_household_income > med_income, 1, 0),
         white = ifelse(ethn == "white",1,0)) %>%
  group_by(age, income, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(income == 0, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(income == 1, white == 0) %>% pull(ballots),
                   df_collapse %>% filter(income == 0, white == 1) %>% pull(ballots),
                   df_collapse %>% filter(income == 1, white == 1) %>% pull(ballots)
)
rejected <- cbind(df_collapse %>% filter(income == 0, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(income == 1, white == 0) %>% pull(rejected),
                  df_collapse %>% filter(income == 0, white == 1) %>% pull(rejected),
                  df_collapse %>% filter(income == 1, white == 1) %>% pull(rejected)
)
data <- list(
  N = as.integer(nrow(rejected)),
  G = 4, 
  submitted = submitted,
  rejected = rejected,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# * Analyze data
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.9)))
df_plot <- data.frame(age = rep(seq(18, 95),4),
                      ethn = c(rep("black", dim(medians)[1] * 2), rep("white", dim(medians)[1] * 2)),
                      income =  rep(c(rep("Below median income", dim(medians)[1]), rep("Above median income", dim(medians)[1])), 2),
                      median = c(medians[,1], medians[,2], medians[,3], medians[,4]),
                      q25 = c(q25[,1], q25[,2],q25[,3], q25[,4]),
                      q75 = c(q75[,1], q75[,2],q75[,3], q75[,4]),
                      q10 = c(q10[,1], q10[,2],q10[,3], q10[,4]),
                      q90 = c(q90[,1], q90[,2],q90[,3], q90[,4]))
# * Plot
ggplot(data = df_plot %>% 
         mutate(income = factor(income, levels = c("Below median income", "Above median income"))), 
       aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by income, race, and age in NC so far",
       color = "Race", 
       fill = "Race") +
  facet_wrap(~income)
ggsave("plots/States/NC/Rejected_rates_by_age_income_race_white_black_NC.jpeg", width = 9, height = 5)

################################################################################
## income, pid------
med_income = median(unique(df$median_household_income))
df_collapse <- df %>%
  filter(age <= 95,
         voter_party_code %in% c("DEM", "REP", "UNA")) %>%
  mutate(outcome = abs(status - 1),
         income = ifelse(median_household_income > med_income, 1, 0),
         pid = ifelse(voter_party_code == "REP",2,
                      ifelse(voter_party_code == "UNA",1,
                             0))) %>%
  group_by(age, income, pid) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(pid == 0, income == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 1, income == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 2, income == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 0, income == 1) %>% pull(ballots),
                   df_collapse %>% filter(pid == 1, income == 1) %>% pull(ballots),
                   df_collapse %>% filter(pid == 2, income == 1) %>% pull(ballots)
)
rejected <- cbind(df_collapse %>% filter(pid == 0, income == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 1, income == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 2, income == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 0, income == 1) %>% pull(rejected),
                  df_collapse %>% filter(pid == 1, income == 1) %>% pull(rejected),
                  df_collapse %>% filter(pid == 2, income == 1) %>% pull(rejected)
)
data <- list(
  N = as.integer(nrow(rejected)),
  G = 6, 
  submitted = submitted,
  rejected = rejected,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 4000)
# * Analyze data
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 90, 
                                  c("Democratic", "Not aligned", "Republican"),
                                  c("Below median income county",
                                    "Above median income county"),
                                  c(),
                                  c("pid", "income"))
# * Plot
ggplot(data = df_plot %>% 
         mutate(income = factor(income, levels = c("Below median income county", "Above median income county")),
                pid = factor(pid, levels = c("Democratic", "Not aligned", "Republican"))), 
       aes(x = age, y = median, color = income)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymin = q25, ymax =  q75, color = income), width = 0, size = 0.75) + 
  geom_errorbar(aes(x = age, ymin = q10, ymax = q90, color = income), width = 0, size = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by income, party id, and age in NC so far",
       color = "Income") +
  facet_wrap(~pid) + 
  theme(legend.position = "bottom")
ggsave("plots/States/NC/Rejected_rates_by_age_income_party_id_NC.jpeg", width = 9, height = 5)

################################################################################
# income and race -----
med_income = median(unique(df$median_household_income))
# * Mangle data
df_collapse <- df %>%
  filter(age <= 95,
         ethn %in% c("white", "black", "hispanic", "asian", "other")) %>%
  mutate(outcome = abs(status - 1),
         ethn_race = ifelse(ethn == "white", 1, 
                     ifelse(ethn == "black", 2, 
                     ifelse(ethn == "hispanic", 3,
                     ifelse(ethn == "asian", 4,
                     ifelse(ethn == "other", 5, NA))))),
         income = ifelse(median_household_income > med_income, 1, 0)) %>%
  arrange(income, ethn_race) %>%
  mutate(ethn_income = group_indices(., paste(income, ethn_race))) %>% 
  group_by(age, ethn_income) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(ethn_income, age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 10, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(ethn_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 4000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 90, 
                                  c("white", "black", "hispanic", "asian", "other"),
                                  c("Below median income county",
                                    "Above median income county"),
                                  c(),
                                  c("ethn", "income"))
# * Plot
ggplot(data = df_plot %>% 
         mutate(ethn = factor(ethn, levels = c("white", "black", "hispanic", "asian", "other")),
                income = factor(income, levels = c("Below median income county", "Above median income county"))), 
       aes(x = age, y = median, color = income)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymin = q25, ymax =  q75, color = income), width = 0, size = 0.75) + 
  geom_errorbar(aes(x = age, ymin = q10, ymax = q90, color = income), width = 0, size = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by income, race, and age in NC so far",
       color = "Income") + 
  theme(legend.position = "bottom") + 
  facet_wrap(.~ethn)
ggsave("plots/States/NC/Rejected_rates_by_age_race_income_NC.jpeg", width = 9, height = 5)

################################################################################
# race and gender ----------
# * Mangle data
df_collapse <- df %>%
  filter(age <= 90,
         ethn %in% c("white", "black", "hispanic", "asian", "other"),
         gender != "U") %>%
  mutate(outcome = abs(status - 1),
         ethn_race = ifelse(ethn == "white", 1, 
                            ifelse(ethn == "black", 2, 
                                   ifelse(ethn == "hispanic", 3,
                                          ifelse(ethn == "asian", 4,
                                                 ifelse(ethn == "other", 5, NA))))),
         gender = ifelse(gender == "M", 1, 0)) %>%
  arrange(gender, ethn_race) %>%
  mutate(ethn_gender = group_indices(., paste(gender, ethn_race))) %>% 
  group_by(age, ethn_gender) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(ethn_gender, age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 10, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(ethn_gender),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 4000)
# * Analyze data
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 90, 
                                             c("white", "black", "hispanic", "asian", "other"),
                                             c("Female",
                                               "Male"),
                                             c(),
                                             c("ethn", "gender"))
# * Plot
ggplot(data = df_plot %>% 
         mutate(ethn = factor(ethn, levels = c("white", "black", "hispanic", "asian", "other")),
                gender = factor(gender, levels = c("Female", "Male"))), 
       aes(x = age, y = median, color = gender)) + 
  geom_point(size = 0.8) + 
  geom_errorbar(aes(x = age, ymin = q25, ymax =  q75, color = gender), width = 0, size = 0.75) + 
  geom_errorbar(aes(x = age, ymin = q10, ymax = q90, color = gender), width = 0, size = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by income, race, and age in NC so far",
       color = "Gender") + 
  theme(legend.position = "bottom") + 
  facet_wrap(.~ethn)
ggsave("plots/States/NC/Rejected_rates_by_age_race_gender_NC.jpeg", width = 12, height = 8)

################################################################################
### male v race v income ------
# * Wrangle data
df_collapse <- df %>%
  filter(age <= 95,
         gender != "U",
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         male = ifelse(gender == "M",1,0),
         white = ifelse(ethn == "white",1,0),
         income = ifelse(zip_median_income < 40000, 1, 
                         ifelse(zip_median_income >= 40000 & zip_median_income < 60000, 2,
                                ifelse(zip_median_income >= 60000, 3, NA)))) %>%
  arrange(gender, white, income) %>%
  mutate(gender_white_income = group_indices(., paste(gender, white, income))) %>% 
  group_by(age, gender_white_income, male, white, income) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(gender_white_income, age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 12, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(gender_white_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 18, 95, 
                                  c("female", "male"), 
                                  c("Black", "white"), 
                                  c("Below 30000",
                                    "30000 - 60000",
                                    "Above 60000"),
                                  c("gender", "ethn", "income"))
# * plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("Below 30000", "30000 - 60000", "Above 60000")))
       , aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity, gender, income, and age in NC so far",
       color = "Race",
       fill = "Race") +
  facet_grid(gender~income)
ggsave("plots/States/NC/Rejected_rates_by_age_gender_race_income_NC.jpeg", width = 9, height = 5)

################################################################################
### pid, race, income -------
#DDDDIIIIRRRR pid
#001100110011 white
#010101010101 income
med_income = median(unique(df$median_household_income))
# * Wrangle data
df_collapse <- df %>%
  filter(age <= 90,
         ethn %in% c("white", "black"),
         voter_party_code %in% c("DEM", "REP")) %>%
  mutate(outcome = abs(status - 1),
         pid = ifelse(voter_party_code == "REP",2, 1),
         white = ifelse(ethn == "white",1,0),
         income = ifelse(zip_median_income < 40000, 1, 
                  ifelse(zip_median_income >= 40000 & zip_median_income < 60000, 2,
                  ifelse(zip_median_income >= 60000, 3, NA)))) %>%
  filter(!is.na(income)) %>%
  arrange(pid, white, income) %>%
  mutate(pid_white_income = group_indices(., paste(pid, white, income))) %>% 
  group_by(age, pid_white_income) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(pid_white_income, age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 12, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(pid_white_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 8000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 18, 90, 
                                  c("Democrat", "Republican"), 
                                  c("Black", "white"), 
                                  c("Below 30000",
                                    "30000 - 60000",
                                    "Above 60000"),
                                  c("pid", "ethn", "income"))
# * Plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("Below 30000", "30000 - 60000", "Above 60000")),
                pid = factor(pid, levels = c("Democrat", "Republican")))
       , aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity, party id, income, and age in NC so far",
       color = "Race",
       fill = "Race") +
  facet_grid(pid~income, scales = "free_y")
ggsave("plots/States/NC/Rejected_rates_by_age_pid_race_income_NC.jpeg", width = 9, height = 5)

# * Plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("Below 30000", "30000 - 60000", "Above 60000")),
                pid = factor(pid, levels = c("Democrat", "Republican")))
       , aes(x = age, y = median, color = income)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = income, fill = income), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = income, fill = income), alpha = 0.25) + 
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text , 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity, party id, income, and age in NC so far",
       color = "Income",
       fill = "Income") +
  facet_grid(pid~ethn, scales = "free_y")
ggsave("plots/States/NC/Rejected_rates_by_age_pid_income_race_NC.jpeg", width = 9, height = 5)


################################################################################
### pid, race, income -------
# * Mangle data
df <- df %>% 
  mutate(zip_income_cat = ifelse(zip_median_income <= 30000, 1, 
                          ifelse(zip_median_income > 30000 & zip_median_income <= 40000, 2,
                          ifelse(zip_median_income > 40000 & zip_median_income <= 50000, 3, 
                          ifelse(zip_median_income > 50000 & zip_median_income <= 60000, 4,
                          ifelse(zip_median_income > 60000, 5, NA))))))     

df_collapse <- df %>%
  filter(age <= 95,
         !is.na(pid),
         !is.na(zip_income_cat),
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  arrange(pid, white, zip_income_cat) %>%
  mutate(pid_white_income = group_indices(., paste(pid, white, zip_income_cat, sep = "_"))) %>% 
  group_by(age, pid_white_income, pid, white, zip_income_cat) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(pid_white_income, age)
# * Model
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 30, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(pid_white_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# * Analyze output
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 18, 95, 
                                  c("DEM", "UNA", "REP"), 
                                  c("Black", "white"), 
                                  c("Below 30000",
                                    "30000-40000",
                                    "40000-50000",
                                    "50000-60000",
                                    "Over 60000"),
                                  c("pid", "ethn", "income"))
# plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("Below 30000", "30000-40000", "40000-50000", "50000-60000", "Over 60000")),
                pid = factor(pid, levels = c("DEM", "UNA", "REP")))
       , aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, ethn = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, ethn = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity income, pid, and age in NC",
       color = "Race",
       fill = "Race") +
  facet_grid(pid~income, scales = "free_y")
ggsave("plots/States/NC/Rejected_rates_by_age_race_zip_income_pid.jpeg", width = 9, height = 5)
################################################################################