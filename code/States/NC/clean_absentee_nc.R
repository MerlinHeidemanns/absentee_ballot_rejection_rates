# libraries
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(boot)
# data
df <- read.csv(file = "data/GE2020/NC/absentee_2020_09_25.csv")
# wrangle
df <- df %>%
  filter(ballot_rtn_status != "RETURNED UNDELIVERABLE",
         ballot_rtn_status != "PENDING") %>%
  mutate(ethn = 
           ifelse(ethnicity == "HISPANIC or LATINO", "hispanic",
           ifelse(race == "WHITE", "white",
           ifelse(race == "BLACK or AFRICAN AMERICAN", "black",
           ifelse(race == "ASIAN", "asian",
           ifelse(race == "INDIAN AMERICAN or ALASKA NATIVE", "native",
           ifelse(race == "NATIVE HAWAIIAN or PACIFIC ISLANDER", "pac", 
                  "other"
                  )))))),
         kind = gsub("[^A-Z]", "", application_num),
         status = ifelse(ballot_rtn_status == "ACCEPTED", 1, 0))
# share submitted, share rejected
submitted <- df %>% 
  group_by(ethn) %>%
  summarize(submitted = n()/nrow(df),
            n = n()) %>%
  ungroup()
rejected <- df %>%
  filter(status == 0) %>%
  group_by(ethn) %>%
  summarize(n = n()) %>% 
  ungroup() %>%
  mutate(rejected = n/sum(n)) %>%
  dplyr::select(ethn, rejected)
joint <- submitted %>% 
  left_join(rejected, by = 'ethn') %>%
  mutate(rejected = ifelse(is.na(rejected), 0, rejected))
ggplot(data = joint %>% 
         filter(ethn != "pac"), aes(x = submitted, y = rejected)) + 
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline() +
  geom_point() +
  theme_bw() +
  geom_text(aes(label=ethn),hjust=-0.25, vjust=-0.5, size = 3) +
  labs(x = "Share submitted", y = "Share rejected", title = paste0("Absentee ballots in NC. Date: ", Sys.Date()))
ggsave("plots/States/NC/Shares_rejected_submitted.jpg")

# rates by age ----------------------------------------------------------------
## all
df_collapse <- df %>%
  mutate(outcome = abs(status - 1)) %>%
  group_by(age) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>%
  mutate(age = factor(age, levels = sort(unique(age))))
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(rejected, ballots) ~ (1 | age), data = df_collapse, 
             family = binomial("logit"), prior = wi_prior)
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
ggplot(data = draws) +
  geom_point(aes(x = g, y = mean)) + 
  geom_errorbar(aes(x = g, ymax = mean + 2/3 * sd, ymin = mean - 2/3 * sd), width = 0) +
  scale_x_continuous(name="Age", limits=c(18, 106)) + 
  labs(caption = "Intervals are 50% certainty intervals.", 
       y = "Rejection probability", 
       title = paste0("Absentee ballots in NC. Date: ", Sys.Date())) + 
  theme_bw()

## white v non-white
df_collapse <- df %>%
  filter(age <= 95) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  group_by(age, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age))))
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(white == 0) %>% pull(ballots),
      df_collapse %>% filter(white == 1) %>% pull(ballots))
rejected <- cbind(df_collapse %>% filter(white == 0) %>% pull(rejected),
                   df_collapse %>% filter(white == 1) %>% pull(rejected))
data <- list(
  N = as.integer(nrow(rejected)),
  submitted = submitted,
  rejected = rejected
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]])
means <- apply(theta, MARGIN = c(2,3), mean)
sds   <- apply(theta, MARGIN = c(2,3), sd)
df_plot <- data.frame(age = rep(seq(18, 95),2),
                 ethn = c(rep("non-white", 96 - 18), rep("white", 96 - 18)),
                 mean = c(means[,1], means[,2]),
                 sd = c(sds[,1], sds[,2]))
ggplot(data = df_plot, aes(x = age, y = mean, color = ethn)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymax = mean + 2/3 * sd, ymin =  mean - 2/3 * sd, color = ethn), width = 0) + 
  theme_bw() +
  labs(x = "Age", 
       y = "Rejection probability", 
       caption = "50% certainty intervals", 
       title = "Estimated rejection rates by ethnicity and age in NC so far",
       color = "Race")
ggsave("plots/States/NC/Rejected_rates_by_age_ethnicity_NC.jpeg", width = 9, height = 5)










