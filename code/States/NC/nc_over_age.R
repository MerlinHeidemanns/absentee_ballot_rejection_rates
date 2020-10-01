source("code/States/NC/clean_absentee_nc.R")
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


## male v female
df_collapse <- df %>%
  filter(age <= 95,
         gender != "U") %>%
  mutate(outcome = abs(status - 1),
         male = ifelse(gender == "M",1,0)) %>%
  group_by(age, male) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age))))
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(male == 0) %>% pull(ballots),
                   df_collapse %>% filter(male == 1) %>% pull(ballots))
rejected <- cbind(df_collapse %>% filter(male == 0) %>% pull(rejected),
                  df_collapse %>% filter(male == 1) %>% pull(rejected))
data <- list(
  N = as.integer(nrow(rejected)),
  G = 2, 
  submitted = submitted,
  rejected = rejected
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]])
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
       y = "Rejection probability", 
       caption = "50% certainty intervals", 
       title = "Estimated rejection rates by ethnicity and age in NC so far",
       color = "Gender")
ggsave("plots/States/NC/Rejected_rates_by_age_gender_NC.jpeg", width = 9, height = 5)


## male v female by race
df_collapse <- df %>%
  filter(age <= 95,
         gender != "U") %>%
  mutate(outcome = abs(status - 1),
         male = ifelse(gender == "M",1,0),
         white = ifelse(ethn == "white",1,0)) %>%
  group_by(age, male, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
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
  rejected = rejected
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
means <- apply(theta, MARGIN = c(2,3), mean)
sds   <- apply(theta, MARGIN = c(2,3), sd)
df_plot <- data.frame(age = rep(seq(18, 95),4),
                      gender = rep(c(rep("female", dim(means)[1]), rep("male", dim(means)[1])),2),
                      ethn = c(rep("non-white", dim(means)[1] * 2), rep("white", dim(means)[1] * 2)),
                      mean = c(means[,1], means[,2], means[,3], means[,4]),
                      sd = c(sds[,1], sds[,2],sds[,3], sds[,4]))
ggplot(data = df_plot, aes(x = age, y = mean, color = gender, shape = ethn)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymax = mean + 2/3 * sd, ymin =  mean - 2/3 * sd, color = gender), width = 0) + 
  theme_bw() +
  labs(x = "Age", 
       y = "Rejection probability (in %)", 
       caption = "50% certainty intervals", 
       title = "Estimated rejection rates by ethnicity and age in NC so far",
       color = "Gender",
       shape = "Race")
ggsave("plots/States/NC/Rejected_rates_by_age_gender_race_NC.jpeg", width = 9, height = 5)
# by pid
df_collapse <- df %>%
  filter(age <= 95,
         voter_party_code %in% c("DEM", "REP", "UNA")) %>%
  mutate(outcome = abs(status - 1),
         pid = ifelse(voter_party_code == "REP",2,
                      ifelse(voter_party_code == "UNA",1,
                             0))) %>%
  group_by(age, pid) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = factor(age, levels = sort(unique(age)))) %>%
  arrange(age)
model <- rstan::stan_model("code/States/NC/age_rw_prior.stan")
submitted <- cbind(df_collapse %>% filter(pid == 0) %>% pull(ballots),
                   df_collapse %>% filter(pid == 1) %>% pull(ballots),
                   df_collapse %>% filter(pid == 2) %>% pull(ballots)
)
rejected <- cbind(df_collapse %>% filter(pid == 0) %>% pull(rejected),
                  df_collapse %>% filter(pid == 1) %>% pull(rejected),
                  df_collapse %>% filter(pid == 2) %>% pull(rejected)
)
data <- list(
  N = as.integer(nrow(rejected)),
  G = 3, 
  submitted = submitted,
  rejected = rejected
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
means <- apply(theta, MARGIN = c(2,3), mean)
sds   <- apply(theta, MARGIN = c(2,3), sd)
df_plot <- data.frame(age = rep(seq(18, 95),3),
                      pid = c(rep("DEM", dim(means)[1]), rep("UNA", dim(means)[1]), rep("REP", dim(means)[1])),
                      mean = c(means[,1], means[,2], means[,3]),
                      sd = c(sds[,1], sds[,2],sds[,3]))
ggplot(data = df_plot, aes(x = age, y = mean, color = pid)) + 
  geom_point() + 
  geom_errorbar(aes(x = age, ymax = mean + 2/3 * sd, ymin =  mean - 2/3 * sd, color = pid), width = 0) + 
  theme_bw() +
  labs(x = "Age", 
       y = "Rejection probability (in %)", 
       caption = "50% certainty intervals", 
       title = "Estimated rejection rates by PID in NC so far",
       color = "PID")
ggsave("plots/States/NC/Rejected_rates_by_age_pid_NC.jpeg", width = 9, height = 5)

