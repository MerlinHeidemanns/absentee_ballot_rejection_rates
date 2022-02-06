################################################################################
## MRP estimates based on NC data for other states
################################################################################
library(tidyverse)
library(rstanarm)
################################################################################
## Load data
# Economist PS framce
df_mrp_dem_abs <- read_rds("data/mrp/2020_ps_frame.rds")
# 2020 NC
source("code/R/States/NC/data_cleaning/clean_absentee_nc_2020.R")
df_2020 <- df
################################################################################
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
# Model
fit <- rstanarm::stan_glmer(cbind(rejected, ballots - rejected) ~ 0 +
                              factor(race) : factor(age) +
                              factor(race) : factor(sex) +
                              factor(age) : factor(sex) + 
                              (1 | race) + (1 | sex) + (1 | age),
                            data = df_mrp, prior = normal(-1, 1), family = binomial(link = "logit"),
                            cores = 4)
# Poststratification
draws = 400
cat <- nrow(df_mrp_dem_abs)
posterior_prob_rej <- t(posterior_linpred(fit, transform = TRUE, draws = draws, newdata = df_mrp_dem_abs))
posterior_prob_lv <- matrix(rnorm(cat * draws, df_mrp_dem_abs$likely_voter, 0.05), nrow = cat, ncol = draws)
posterior_prob_dem <- matrix(rnorm(cat * draws, df_mrp_dem_abs$pres_2020_dem, 0.05), nrow = cat, ncol = draws)
posterior_prob_rep <- matrix(rnorm(cat * draws, df_mrp_dem_abs$pres_2020_rep, 0.05), nrow = cat, ncol = draws)
posterior_prob_abs_dem <- matrix(inv.logit(rnorm(cat * draws, 1 + logit(df_mrp_dem_abs$absentee_vote), 0.2)), nrow = cat, ncol = draws)
posterior_prob_abs_rep <- matrix(inv.logit(rnorm(cat * draws, -1 + logit(df_mrp_dem_abs$absentee_vote), 0.2)), nrow = cat, ncol = draws)

dem_adj = posterior_prob_lv * posterior_prob_dem * posterior_prob_abs_dem * (1 - posterior_prob_rej) + 
          posterior_prob_lv * posterior_prob_dem * (1 - posterior_prob_abs_dem)
rep_adj = posterior_prob_lv * posterior_prob_rep * posterior_prob_abs_rep * (1 - posterior_prob_rej) + 
          posterior_prob_lv * posterior_prob_rep * (1 - posterior_prob_abs_rep)

dem_unadj = posterior_prob_lv * posterior_prob_dem
rep_unadj = posterior_prob_lv * posterior_prob_rep
# Prediction by state
states <- unique(df_mrp_dem_abs$state_abb)
out <- matrix(NA, ncol = 3, nrow = 0)
for (i in states){
  # adj
  dem_adj_i <- dem_adj[df_mrp_dem_abs$state_abb == i, ]
  rep_adj_i <- rep_adj[df_mrp_dem_abs$state_abb == i, ]
  n_i <- df_mrp_dem_abs$n[df_mrp_dem_abs$state_abb == i]
  dem_votes <- rowSums(t(dem_adj_i) %*% n_i)
  rep_votes <- rowSums(t(rep_adj_i) %*% n_i)
  dem_share_adj <- dem_votes/(dem_votes + rep_votes)
  out <- rbind(out, cbind(i, dem_share_adj, "adj"))
  # unadj
  dem_unadj_i <- dem_unadj[df_mrp_dem_abs$state_abb == i, ]
  rep_unadj_i <- rep_unadj[df_mrp_dem_abs$state_abb == i, ]
  dem_votes <- rowSums(t(dem_unadj_i) %*% n_i)
  rep_votes <- rowSums(t(rep_unadj_i) %*% n_i)
  dem_share_unadj <- dem_votes/(dem_votes + rep_votes)
  out <- rbind(out, cbind(i, dem_share_unadj, "unadj"))
}
out <- as.data.frame(out)
colnames(out) <- c("state", "share", "kind")
out <- out %>%
  mutate(share = as.numeric(as.character(share)))
################################################################################
## Plot
ggplot(data = out, aes(x = share, color = kind, fill = kind)) + 
  geom_histogram(alpha = 0.5, position = "identity") + 
  facet_wrap(.~state, scales = "free_x") + 
  theme_bw() 

out_coll <- out %>% 
  group_by(state, kind) %>%
  summarize(q25 = quantile(share, 0.25),
            q75 = quantile(share, 0.75),
            q975 = quantile(share, 0.975),
            q025 = quantile(share, 0.025),
            q50 = quantile(share, 0.5), 
            sd = sd(share))
order <- out_coll %>% filter(kind == "unadj") %>% arrange(-q50) %>% pull(state)
  
## Plot
ggplot(data = out_coll %>% 
         mutate(state = factor(state, levels = order)) %>%
         filter(state != "DC"),
       aes(x = state, y = q50, color = kind)) + 
  geom_point(position = 
               position_dodge(0.5), size = 0.5) + 
  geom_errorbar(aes(x = state, ymax = q75,  ymin = q25,  color = kind), size = 1,   
                position = position_dodge(0.5), width = 0) + 
  geom_errorbar(aes(x = state, ymax = q975, ymin = q025, color = kind), size = 0.5, 
                position = position_dodge(0.5), width = 0) + 
  theme_bw() + 
  coord_flip()
################################################################################





# 
# df_mrp_dem_abs <- df_mrp_dem_abs %>%
#   mutate(n_lv = floor(n * likely_voter))
# absentee_voter_share_dem <- (1 - posterior_prob) * 
#   matrix(rep(df_mrp_dem_abs$pres_2020_dem * df_mrp_dem_abs$absentee_vote, draws), nrow = draws, byrow = TRUE)
# in_person_voter_share_dem <- (df_mrp_dem_abs$pres_2020_dem) * (1 - df_mrp_dem_abs$absentee_vote)
# hist(colMeans(absentee_voter_share_dem) + in_person_voter_share_dem - df_mrp_dem_abs$pres_2020_dem, breaks = 200)
# 
# absentee_voter_share_rep <- (1 - posterior_prob) * matrix(rep(df_mrp_dem_abs$pres_2020_rep * df_mrp_dem_abs$absentee_vote, draws), nrow = draws, byrow = TRUE)
# in_person_voter_share_rep <- (df_mrp_dem_abs$pres_2020_rep) * (1 - df_mrp_dem_abs$absentee_vote)
# 
# hist(colMeans(absentee_voter_share_rep) + 
#        in_person_voter_share_rep - df_mrp_dem_abs$pres_2020_rep, breaks = 200)
# df <- data.frame(rep_adj = colMeans(absentee_voter_share_rep) + in_person_voter_share_rep,
#                  rep_unadj = df_mrp_dem_abs$pres_2020_rep,
#                  dem_adj = colMeans(absentee_voter_share_dem) + in_person_voter_share_dem ,
#                  dem_unadj = df_mrp_dem_abs$pres_2020_dem)
# df <- cbind(df, df_mrp_dem_abs)
# df <- df %>% mutate(
#   dem_adj_n = n_lv * dem_adj,
#   dem_unadj_n = n_lv * dem_unadj,
#   rep_adj_n = n_lv * rep_adj,
#   rep_unadj_n = n_lv * rep_unadj,
# ) %>%
#   group_by(state_abb) %>%
#   summarize(n_state = sum(n),
#             n_dem_adj = sum(dem_adj_n),
#             n_dem_unadj = sum(dem_unadj_n),
#             n_rep_adj = sum(rep_adj_n),
#             n_rep_unadj = sum(rep_unadj_n)
#   ) %>%
#   mutate(rep_adj_voteshare = n_rep_adj/n_state,
#          rep_unadj_voteshare = n_rep_unadj/n_state,
#          dem_adj_voteshare = n_dem_adj/n_state,
#          dem_unadj_voteshare = n_dem_unadj/n_state) %>%
#   select(state_abb, rep_adj_voteshare, rep_unadj_voteshare, 
#          dem_adj_voteshare, dem_unadj_voteshare) %>%
#   mutate(diff_adj = dem_adj_voteshare - rep_adj_voteshare,
#          diff_unadj = dem_unadj_voteshare - rep_unadj_voteshare)
# ggplot(data = df, aes(x = 100 * (rep_unadj_voteshare - rep_adj_voteshare),
#                       y = 100 * (dem_unadj_voteshare - dem_adj_voteshare))) + 
#   geom_text(aes(label = state_abb), size = 3) + 
#   geom_abline() + 
#   lims(x = c(0, 0.75),
#        y = c(0, 0.75)) + 
#   labs(x = "Republican loss (in %)",
#        y = "Democratic loss (in %)") +
#   theme_bw()
# ggsave("plots/mrp/dem_rep_mean_loss.jpeg", width = 10, height = 6)

