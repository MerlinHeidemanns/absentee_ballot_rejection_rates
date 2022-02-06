library(tidyverse)
library(rstanarm)
df <- read_delim("data/GE2020/ME/maine_primary_june.txt", delim = "|")

df <- df %>%
  mutate(status = ifelse(`ACC OR REJ` == "ACC", 1,
                  ifelse(`ACC OR REJ` == "REJ", 0, NA)),
         party = ifelse(P == "D", "DEM",
                 ifelse(P == "R", "REP",
                        "OTHER")        
                  ),
         outcome = abs(status - 1)) %>%
  filter(!is.na(status))
df_collapsed <- df %>%
  group_by(P) %>%
  summarize(ballots = n(),
            rejected = sum(outcome))
# model
prior = normal(-1, 1)
fit <- stan_glmer(cbind(rejected, ballots - rejected) ~ (1 | P), data = df_collapsed, prior = prior, cores = 4,
                family = binomial("logit"))
draws <- fit %>%
  spread_draws(b[t,g], `(Intercept)`) %>%
  dplyr::select(g, b, `(Intercept)`) %>%
  group_by(g) %>%
  mutate(b = inv.logit(b + `(Intercept)`)) %>%
  summarize(median = median(b),
            q25 = quantile(b, c(0.25)),
            q75 = quantile(b, c(0.75)),
            q025 = quantile(b, c(0.025)),
            q975 = quantile(b, c(0.975))) %>%
  ungroup() %>%
  mutate(g = gsub("P:", "", g),
         g = ifelse(g == "D", "Democrat",
             ifelse(g == "R", "Republican",
             ifelse(g == "G", "Green", "Unaffiliated")))
  )
ggplot(data = draws) +
  geom_point(aes(x = g, y = median)) + 
  geom_errorbar(aes(x = g, ymax = q75, ymin = q25), width = 0, size = 1) +
  geom_errorbar(aes(x = g, ymax = q975, ymin = q025), width = 0, size = 0.5) +
  labs(caption = "Thick line: 50%
                  Thin line: 95%", 
       y = "Rejection probability", 
       title = paste0("Absentee ballots in ME. Primaries June 2020")) + 
  theme_bw() + 
  theme(axis.title.x = element_blank())
ggsave("plots/States/ME/Rejected_rates_by_party_id_primaries.jpeg", width = 9, height = 5)


