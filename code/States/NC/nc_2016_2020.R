# text
caption_text = "Line: Median
                  Inner band: 50%
                  Outer band: 80%"
y_text = "Rejection probability (in %)"
# load
source("code/States/NC/clean_absentee_nc.R")
df_2020 <- df
source("code/States/NC/clean_absentee_nc_2016.R")
df_2016 <- df
source("code/States/NC/clean_absentee_nc_2018.R")
df_2018 <- df


df_2020 <- df_2020 %>% 
  select(median_household_income, status, ethn, age) %>%
  mutate(year = 2)
df_2018 <- df_2018 %>% 
  select(median_household_income, status, ethn, age) %>%
  mutate(year = 1)
df_2016 <- df_2016 %>% 
  select(median_household_income, status, ethn, age) %>%
  mutate(year = 0)
df <- bind_rows(df_2016, df_2018)
df <- bind_rows(df, df_2020)



### income v race v year ------
med_income = median(unique(df$median_household_income))
df_collapse <- df %>%
  filter(age <= 95,
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0),
         income = ifelse(median_household_income > med_income, 1, 0)) %>%
  arrange(year, white, income) %>%
  mutate(year_white_income = group_indices(., paste(year, white, income))) %>% 
  group_by(age, year_white_income, year, white, income) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(year_white_income, age)
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 12, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(year_white_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# prepare
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 95, 
                                  c("2016", "2018", "2020"), 
                                  c("Black", "white"), 
                                  c("Below median income county",
                                    "Above median income county"),
                                  c("year", "ethn", "income"))
# plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("Below median income county", "Above median income county")),
                year = factor(year, levels = c("2016", "2018", "2020")))
       , aes(x = age, y = median, color = year)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = year, fill = year), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = year, fill = year), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity income, and age comparing 2016 and 2020 in NC",
       color = "Election",
       fill = "Election") +
  facet_grid(income~ethn)
ggsave("plots/States/NC/Rejected_rates_by_age_race_income_2016v2020_NC.jpeg", width = 9, height = 5)



