# libraries
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(boot)
library(ggmap)
# load
df <- read_csv("data/GE2020/NC/absentee_2020_10_05_w_2016_18.csv")

# base model
df_collapse <- df %>%
  filter(ethn %in% c("white", "black", "hispanic", "asian", "other")) %>%
  mutate(outcome = abs(status - 1)) %>%
  group_by(ethn, voted_by_mail_last_four_years) %>%
  summarize(ballots = n(),
            rejected = sum(outcome))
wi_prior <- normal(-1, 0.5)

fit <- rstanarm::stan_glm(cbind(rejected, ballots - rejected) ~ ethn*voted_by_mail_last_four_years, data = df_collapse,
                          family = binomial("logit"), prior = wi_prior, cores = 4)



# yes no
df_collapse <- df %>%
  filter(age <= 90,
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         ethn_race = ifelse(ethn == "white", 1, 0)) %>%
  arrange(gender, ethn_race) %>%
  mutate(ethn_voted = group_indices(., paste(voted_by_mail_last_four_years, ethn_race))) %>% 
  group_by(age, ethn_voted) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(ethn_voted, age)

model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 4, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(ethn_voted),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 4000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.9)))
df_plot <- data.frame(age = rep(seq(18, 90),4),
                      ethn =  rep(c(rep("black", dim(medians)[1]), 
                                    rep("white", dim(medians)[1])), 2),
                      voted =  c(rep("No", dim(medians)[1] * 2), 
                                  rep("Yes", dim(medians)[1] * 2)),
                      median = c(medians[,1], medians[,2], medians[,3], medians[,4]),
                      q25 = c(q25[,1], q25[,2],q25[,3], q25[,4]),
                      q75 = c(q75[,1], q75[,2],q75[,3], q75[,4]),
                      q10 = c(q10[,1], q10[,2],q10[,3], q10[,4]),
                      q90 = c(q90[,1], q90[,2],q90[,3], q90[,4]))
ggplot(data = df_plot %>% 
         mutate(ethn = factor(ethn, levels = c("white", "black")),
                voted = factor(voted, levels = c("No", "Yes"))), 
       aes(x = age, y = median, color = voted)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = voted, fill = voted), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = voted, fill = voted), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = "Rejection probability (in %)", 
       caption = "Line: Median
                  Inner band: 50%
                  Outer band: 80%
                  Prior absentee voting refers to previous four years", 
       title = "Estimated rejection rates by first time absentee voters, race, and age in NC so far",
       color = "Voted absentee before",
       fill = "Voted absentee before") + 
  theme(legend.position = "bottom") + 
  facet_wrap(.~ethn)
ggsave("plots/States/NC/Rejected_rates_by_age_ethnicity_first_time_NC.jpeg", width = 9, height = 5)

# + income -----
# yes no
med_income = median(unique(df$median_household_income))

df_collapse <- df %>%
  filter(age <= 90,
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         income = ifelse(median_household_income > med_income, 1, 0),
         white = ifelse(ethn == "white", 1, 0)) %>%
  arrange(voted_by_mail_last_four_years, white, income) %>%
  mutate(voted_white_income = group_indices(., paste(voted_by_mail_last_four_years, white, income))) %>% 
  group_by(age, voted_white_income) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(voted_white_income, age)

model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 8, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(voted_white_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 4000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
medians <- apply(theta, MARGIN = c(2,3), median)
q25   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.25)))
q75   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.75)))
q10   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.10)))
q90   <- apply(theta, MARGIN = c(2,3), function(x) quantile(x, c(0.9)))
df_plot <- data.frame(age = rep(seq(18, 90),8),
                      ethn =  rep(c(rep("black", dim(medians)[1] * 2), 
                                    rep("white", dim(medians)[1] * 2)), 2),
                      voted =  c(rep("No", dim(medians)[1] * 4), 
                                 rep("Yes", dim(medians)[1] * 4)),
                      income =  rep(c(rep("Below median income county", dim(medians)[1]), 
                                      rep("Above median income county", dim(medians)[1])), 4),  
                      median = c(medians[,1], medians[,2], medians[,3], medians[,4],
                                 medians[,5], medians[,6], medians[,7], medians[,8]),
                      q25 = c(q25[,1], q25[,2],q25[,3], q25[,4],
                              q25[,5], q25[,6],q25[,7], q25[,8]),
                      q75 = c(q75[,1], q75[,2],q75[,3], q75[,4],
                              q75[,5], q75[,6],q75[,7], q75[,8]),
                      q10 = c(q10[,1], q10[,2],q10[,3], q10[,4],
                              q10[,5], q10[,6],q10[,7], q10[,8]),
                      q90 = c(q90[,1], q90[,2],q90[,3], q90[,4],
                              q90[,5], q90[,6],q90[,7], q90[,8]))
ggplot(data = df_plot %>% 
         mutate(ethn = factor(ethn, levels = c("white", "black")),
                voted = factor(voted, levels = c("No", "Yes")),
                income = factor(income, levels = c("Below median income county", "Above median income county"))), 
       aes(x = age, y = median, color = voted)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = voted, fill = voted), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = voted, fill = voted), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = "Rejection probability (in %)", 
       caption = "Line: Median
                  Inner band: 50%
                  Outer band: 80%
                  Prior absentee voting refers to previous four years", 
       title = "Estimated rejection rates by first time absentee voters, race, and age in NC so far",
       color = "Voted absentee before",
       fill = "Voted absentee before") + 
  theme(legend.position = "bottom") + 
  facet_grid(ethn~income, scales = "free_y")
ggsave("plots/States/NC/Rejected_rates_by_age_ethnicity_first_time_income_NC.jpeg", width = 9, height = 5)


