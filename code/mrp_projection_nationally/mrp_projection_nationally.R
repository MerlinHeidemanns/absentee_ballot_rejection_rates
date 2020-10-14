# mrp estimates
# mrp Elliott
df_mrp_dem_abs <- read_rds("data/mrp/2020_ps_frame.rds")
  
# 2020
source("code/States/NC/clean_absentee_nc.R")
df_2020 <- df
# create variables
df_mrp <- df_2020 %>%
  filter(gender %in% c("F", "M")) %>%
  mutate(age = ifelse(age >= 18 & age <= 29, "18-29", 
                   ifelse(age >= 30 & age <= 44, "30-44",
                   ifelse(age >= 45 & age <= 64, "45-64", 
                                                 "65+"))),
         sex = ifelse(gender == "M", "Male", "Female"),
         race = ifelse(ethn == "black", "Black, Non-Hispanic",
                ifelse(ethn == "white", "White, Non-Hispanic",
                ifelse(ethn == "hispanic", "Hispanic", "Other, Non-Hispanic"))),
         outcome = abs(status - 1)
  ) %>%
  group_by(race, sex, age) %>%
  summarize(ballots = n(),
            rejected = sum(outcome))
# estimates
fit <- rstanarm::stan_glmer(cbind(rejected, ballots - rejected) ~ 0 +
                              factor(race) : factor(age) +
                              factor(race) : factor(sex) +
                              factor(age) : factor(sex) + 
                              (1 | race) + (1 | sex) + (1 | age),
                            data = df_mrp, prior = normal(-1, 1), family = binomial(link = "logit"),
                            cores = 4)
# rej prob
draws = 100
posterior_prob <- rstanarm::posterior_linpred(fit, transform = TRUE, draws = draws, newdata = df_mrp_dem_abs)

absentee_voter_share_dem <- (1 - posterior_prob) * matrix(rep(df_mrp_dem_abs$pres_2020_dem * df_mrp_dem_abs$absentee_vote, draws), nrow = draws, byrow = TRUE)
in_person_voter_share_dem <- (df_mrp_dem_abs$pres_2020_dem) * (1 - df_mrp_dem_abs$absentee_vote)
hist(colMeans(absentee_voter_share_dem) + in_person_voter_share_dem - df_mrp_dem_abs$pres_2020_dem, breaks = 200)

absentee_voter_share_rep <- (1 - posterior_prob) * matrix(rep(df_mrp_dem_abs$pres_2020_rep * df_mrp_dem_abs$absentee_vote, draws), nrow = draws, byrow = TRUE)
in_person_voter_share_rep <- (df_mrp_dem_abs$pres_2020_rep) * (1 - df_mrp_dem_abs$absentee_vote)

hist(colMeans(absentee_voter_share_rep) + 
       in_person_voter_share_rep - df_mrp_dem_abs$pres_2020_rep, breaks = 200)
df <- data.frame(rep_adj = colMeans(absentee_voter_share_rep) + in_person_voter_share_rep,
                 rep_unadj = df_mrp_dem_abs$pres_2020_rep,
                 dem_adj = colMeans(absentee_voter_share_dem) + in_person_voter_share_dem ,
                 dem_unadj = df_mrp_dem_abs$pres_2020_dem)
df <- cbind(df, df_mrp_dem_abs)
df <- df %>% mutate(
  dem_adj_n = n * dem_adj,
  dem_unadj_n = n * dem_unadj,
  rep_adj_n = n * rep_adj,
  rep_unadj_n = n * rep_unadj,
) %>%
  group_by(state_abb) %>%
  summarize(n_state = sum(n),
            n_dem_adj = sum(dem_adj_n),
            n_dem_unadj = sum(dem_unadj_n),
            n_rep_adj = sum(rep_adj_n),
            n_rep_unadj = sum(rep_unadj_n)
  ) %>%
  mutate(rep_adj_voteshare = n_rep_adj/n_state,
         rep_unadj_voteshare = n_rep_unadj/n_state,
         dem_adj_voteshare = n_dem_adj/n_state,
         dem_unadj_voteshare = n_dem_unadj/n_state) %>%
  select(state_abb, rep_adj_voteshare, rep_unadj_voteshare, 
         dem_adj_voteshare, dem_unadj_voteshare) %>%
  mutate(diff_adj = dem_adj_voteshare - rep_adj_voteshare,
         diff_unadj = dem_unadj_voteshare - rep_unadj_voteshare)
ggplot(data = df, aes(x = 100 * (rep_unadj_voteshare - rep_adj_voteshare),
                      y = 100 * (dem_unadj_voteshare - dem_adj_voteshare))) + 
  geom_text(aes(label = state_abb), size = 3) + 
  geom_abline() + 
  lims(x = c(0, 1),
       y = c(0, 1)) + 
  labs(x = "Republican loss (in %)",
       y = "Democratic loss (in %)") +
  theme_bw()
ggsave("plots/mrp/dem_rep_mean_loss.jpeg", width = 10, height = 6)

