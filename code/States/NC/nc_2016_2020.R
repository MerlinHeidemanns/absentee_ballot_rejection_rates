# functions
source("code/States/NC/functions.R")
# text
caption_text = "Line: Median
                  Inner band: 50%
                  Outer band: 80%"
y_text = "Rejection probability (in %)"
# load
source("code/States/NC/clean_absentee_nc.R")
df_2020 <- df
source("code/States/NC/clean_absentee_nc_2018.R")
df_2018 <- df
source("code/States/NC/clean_absentee_nc_2016.R")
df_2016 <- df

df_2020 <- df_2020 %>% 
  select(median_household_income, status, ethn, age, pid) %>%
  mutate(year = 2)
df_2018 <- df_2018 %>% 
  filter(ballot_rtn_status != "RETURNED UNDELIVERABLE",
         ballot_rtn_status != "PENDING",
         ballot_rtn_status != "SPOILED",
         ballot_req_type == "MAIL",
         ballot_rtn_status != "") %>%
  select(median_household_income, status, ethn, age, pid) %>%
  mutate(year = 1)
df_2016 <- df_2016 %>%
  filter(ballot_rtn_status != "RETURNED UNDELIVERABLE",
         ballot_rtn_status != "PENDING",
         ballot_rtn_status != "SPOILED",
         ballot_req_type == "MAIL",
         ballot_rtn_status != ""
  ) %>%
  select(median_household_income, status, ethn, age, pid) %>%
  mutate(year = 0)
df <- bind_rows(df_2016, df_2018)
df <- bind_rows(df, df_2020)



### income v race v year ------
med_income = mean(df$median_household_income)
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
  prior_sigma = 1
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
ggsave("plots/States/NC/Rejected_rates_by_age_race_income_2016v2018v2020_NC.jpeg", width = 9, height = 5)

### race v year ------
df_collapse <- df %>%
  filter(age <= 95,
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  arrange(year, white) %>%
  mutate(year_white = group_indices(., paste(year, white))) %>% 
  group_by(age, year_white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  ungroup() %>% 
  #distinct(year_white, year, white) %>% View()
  # year  001122 2016  2016  2018  2018  2020  2020
  # white 010101 black white black white black white
  mutate(age = as.integer(age)) %>%
  arrange(year_white, age)
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 6, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(year_white),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# prepare
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 95, 
                                  c("2016", "2018", "2020"), 
                                  c("Black", "white"), 
                                  c(),
                                  c("year", "ethn"))
# plot
ggplot(data = df_plot %>%
         mutate(year = factor(year, levels = c("2016", "2018", "2020")))
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
  facet_grid(~ethn)
ggsave("plots/States/NC/Rejected_rates_by_age_race_2016v2018v2020_NC.jpeg", width = 9, height = 5)







## pid v race v year -----
df_collapse <- df %>%
  filter(age <= 95,
         ethn %in% c("white", "black"),
         !is.na(pid)) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  arrange(year, white, pid) %>%
  mutate(year_white_pid = group_indices(., paste(year, white, pid))) %>% 
  group_by(age, year_white_pid) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(year_white_pid, age)
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 18, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(year_white_pid),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# prepare
111111222222333333
000111000111000111
012012012012012012
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, c(2,3), 95, 
                                  c("2016", "2018", "2020"), 
                                  c("Black", "white"), 
                                  c("Democrat",
                                    "Unaffiliated",
                                    "Republican"),
                                  c("year", "ethn", "pid"))
# plot
ggplot(data = df_plot %>%
         mutate(pid = factor(pid, levels = c("Democrat", "Unaffiliated", "Republican")),
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
  facet_grid(pid~ethn, scales = "free_y")
ggsave("plots/States/NC/Rejected_rates_by_age_pid_race_2016v2018v2020_NC.jpeg", width = 9, height = 5)
# plot
ggplot(data = df_plot %>%
         mutate(pid = factor(pid, levels = c("Democrat", "Unaffiliated", "Republican")),
                year = factor(year, levels = c("2016", "2018", "2020")))
       , aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity income, and age comparing 2016 and 2020 in NC",
       color = "Race",
       fill = "Race") +
  facet_grid(pid~year, scales = "free_y")




# race x income x age -----


# load
source("code/States/NC/clean_absentee_nc.R")
df_2020 <- df
source("code/States/NC/clean_absentee_nc_2016.R")
df_2016 <- df

df_2020 <- df_2020 %>% 
  select(zip_median_income, status, ethn, age, pid) %>%
  mutate(year = 1)
df_2016 <- df_2016 %>%
  filter(ballot_rtn_status != "RETURNED UNDELIVERABLE",
         ballot_rtn_status != "PENDING",
         ballot_rtn_status != "SPOILED",
         ballot_req_type == "MAIL",
         ballot_rtn_status != ""
  ) %>%
  select(zip_median_income, status, ethn, age, pid) %>%
  mutate(year = 0)
df <- bind_rows(df_2016, df_2020)

df <- df %>% 
  mutate(zip_income_cat = ifelse(zip_median_income > 20000 & zip_median_income <= 35000, 1, 
                          ifelse(zip_median_income > 35000 & zip_median_income <= 50000, 2,
                          ifelse(zip_median_income > 50000 & zip_median_income <= 65000, 3, 
                          ifelse(zip_median_income > 65000 & zip_median_income <= 80000, 4,
                                 NA)))))     
                                 
df_collapse <- df %>%
  filter(age <= 95,
         !is.na(zip_income_cat),
         ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  arrange(year, white, zip_income_cat) %>%
  mutate(year_white_income = group_indices(., paste(year, white, zip_income_cat))) %>% 
  group_by(age, year_white_income, year, white, zip_income_cat) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(age = as.integer(age)) %>%
  arrange(year_white_income, age)
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = length(unique(df_collapse$age)),
  G = 16, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(year_white_income),
  a = df_collapse %>% pull(age) - 17,
  prior_mu = 1,
  prior_sigma = 1
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# prepare
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
# 000000111111
# 000111000111
# 012012012012
df_plot <- extract_quantiles_mean(theta, c(2,3), 18, 95, 
                                  c("2016", "2020"), 
                                  c("Black", "white"), 
                                  c("20000-35000",
                                    "35000-50000",
                                    "50000-65000",
                                    "65000-80000"),
                                  c("year", "ethn", "income"))
# plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("20000-35000", "35000-50000", "50000-65000", "65000-80000")),
                year = factor(year, levels = c("2016", "2020")))
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
  facet_grid(ethn~income)
ggsave("plots/States/NC/Rejected_rates_by_age_race_zip_income_2016v2020_NC.jpeg", width = 9, height = 5)
# plot
ggplot(data = df_plot %>%
         mutate(income = factor(income, levels = c("20000-35000", "35000-50000", "50000-65000", "65000-80000")),
                year = factor(year, levels = c("2016", "2020")))
       , aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  labs(x = "Age", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity income, and age comparing 2016 and 2020 in NC",
       color = "Race",
       fill = "Race") +
  facet_grid(year~income)
ggsave("plots/States/NC/Rejected_rates_by_age_zip_income_race_2016v2020_NC.jpeg", width = 9, height = 5)


