# over income
source("code/States/NC/clean_absentee_nc.R")



df <- df %>% 
  mutate(zip_income_cat = ifelse(zip_median_income < 20000, 1, 
                                 1 + ceiling((zip_median_income - 20000)/1000)))

### income v race v year ------
df_collapse <- df %>%
  filter(ethn %in% c("white", "black")) %>%
  mutate(outcome = abs(status - 1),
         white = ifelse(ethn == "white",1,0)) %>%
  arrange(white) %>%
  mutate(white = group_indices(., paste(white))) %>% 
  group_by(zip_income_cat, white) %>%
  summarize(ballots = n(),
            rejected = sum(outcome)) %>% 
  mutate(zip_income_cat = as.integer(zip_income_cat)) %>%
  arrange(white, zip_income_cat)
model <- rstan::stan_model("code/States/NC/age_rw_prior_mat.stan")
data <- list(
  N = nrow(df_collapse),
  A = max(df_collapse$zip_income_cat),
  G = 2, 
  submitted = df_collapse %>% pull(ballots),
  rejected = df_collapse %>% pull(rejected),
  g = df_collapse %>% pull(white),
  a = df_collapse %>% pull(zip_income_cat),
  prior_mu = 1,
  prior_sigma = 0.5
)
fit <- rstan::sampling(model, data = data, chains = 4, cores = 4, warmup = 4000, iter = 6000)
# prepare
theta <- inv.logit(rstan::extract(fit, pars = "theta")[[1]]) * 100
df_plot <- extract_quantiles_mean(theta, dimensions = c(2,3), lower_bound = 1, upper_bound = data$A, 
                                  cat1_factors = c("Black", "white"), 
                                  cat2_factors = c(),
                                  cat3_factors = c(),
                                  cat_names = c("ethn"))
# plot
ggplot(data = df_plot, aes(x = age, y = median, color = ethn)) + 
  geom_line(size = 1) + 
  geom_ribbon(aes(x = age, ymax = q25, ymin =  q75, color = ethn, fill = ethn), alpha = 0.5) + 
  geom_ribbon(aes(x = age, ymax = q10, ymin = q90, color = ethn, fill = ethn), alpha = 0.25) + 
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 81, 10), labels = seq(20000, 20000 + 81 * 1000, 10 * 1000)) +
  labs(x = "Income brackets", 
       y = y_text, 
       caption = caption_text, 
       title = "Estimated rejection rates by ethnicity and income in NC",
       color = "Race",
       fill = "Race") 





