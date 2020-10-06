source("code/States/NC/clean_absentee_nc.R")
intervals <- 10
df_collapse <- df %>%
  mutate(outcome = abs(status - 1),
         white = factor(ifelse(ethn == "white",1, 0)),
         income_quint = factor(cut_interval(median_household_income, intervals, labels = FALSE))) %>%
  group_by(income_quint, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome))

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
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 2000, iter = 3000)
# plot
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]])
means <- apply(theta, MARGIN = c(2,3), mean)
sds   <- apply(theta, MARGIN = c(2,3), sd)
df_plot <- data.frame(income = rep(levels(cut_interval(df$median_household_income, intervals)),2),
                      ethn = c(rep("black", intervals), rep("white", intervals)),
                      mean = c(means[,1], means[,2]),
                      sd = c(sds[,1], sds[,2]))
ggplot(data = df_plot, aes(x = income, y = mean, color = ethn)) + 
  geom_point() + 
  geom_errorbar(aes(x = income, ymax = mean + 2/3 * sd, ymin =  mean - 2/3 * sd, color = ethn), width = 0) + 
  theme_bw() +
  labs(x = "Median income", 
       y = "Rejection probability", 
       caption = "50% certainty intervals", 
       title = "Estimated rejection rates by ethnicity and age in NC so far",
       color = "Race")

