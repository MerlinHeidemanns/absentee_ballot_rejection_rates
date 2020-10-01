source("code/States/NC/clean_absentee_nc.R")
df_coll_rejected <- df_rejected %>%
  mutate(rejected = 1) %>%
  group_by(ethn) %>%
  summarize(rejected = sum(rejected),
            requested_cure = sum(request_cure))
# model
wi_prior <- normal(-1, 0.5)
fit <- 
  stan_glmer(cbind(requested_cure, rejected - requested_cure) ~ (1 | ethn), data = df_coll_rejected, 
             family = binomial("logit"), prior = wi_prior)
draws <- fit %>%
  spread_draws(b[t,g], `(Intercept)`) %>%
  dplyr::select(g, b, `(Intercept)`) %>%
  group_by(g) %>%
  mutate(b = inv.logit(b + `(Intercept)`) * 100) %>%
  summarize(mean = mean(b),
            sd = sd(b)) %>%
  ungroup() %>%
  mutate(g = gsub("ethn:", "", g))

true_val <- df_coll_rejected %>%
  mutate(mean = requested_cure/rejected,
         sd   = sqrt(mean * (1 - mean)/rejected))

ggplot(data = draws %>% 
         mutate(g = factor(g, levels = g[order(mean)])), aes(x = g, y = mean)) + 
  geom_point() + 
  geom_errorbar(aes(x = g, ymax = mean + 2/3 * sd, ymin = mean - 2/3 * sd), width = 0) +
  theme_bw() + 
  labs(caption = "50% certainty intervals", y = "Cure request probability (in %)",
       title = "Probability to request to cure ballot in NC so far") + 
  theme(axis.title.x = element_blank())
ggsave("plots/States/NC/Cure_probability.jpeg")









  