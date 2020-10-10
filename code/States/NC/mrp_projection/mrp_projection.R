# mrp estimates
source("code/States/NC/clean_absentee_nc.R")
#
df_2020 <- df
# create variables
df_mrp <- df_2020 %>%
  mutate(mrp_ethn = ifelse(ethn == "native" | ethn == "pac" | ethn == "asian", "other", ethn),
         mrp_age = ifelse(age < 25, "Below 25", 
                   ifelse(age < 40, "Below 40",
                   ifelse(age < 55, "Below 55", "Above equal 55"))),
         mrp_gender = gender,
         mrp_pid    = ifelse(voter_party_code %in% c("DEM", "REP"), voter_party_code, "Other"),
         mrp_income_cat = ifelse(zip_median_income <= 40000, "Below 40000", 
                          ifelse(zip_median_income > 40000 & zip_median_income <= 60000, "Bet 40 and 50",
                          ifelse(zip_median_income > 60000, "Above 60000", 4))),
         outcome = abs(status - 1)
         ) %>%
  group_by(mrp_ethn, mrp_age, mrp_gender, mrp_pid, mrp_income_cat) %>%
  filter(mrp_gender != "U") %>%
  summarize(ballots = n(),
            rejected = sum(outcome))
table(df_mrp$mrp_gender)
table(df_mrp$mrp_pid,df_mrp$mrp_ethn)
table(df_mrp$mrp_age)
table(df_mrp$mrp_ethn, df_mrp$mrp_income_cat, df_mrp$mrp_pid, df_mrp$mrp_gender)
# estimates
fit <- rstanarm::stan_glmer(cbind(rejected, ballots - rejected) ~ 
                              factor(mrp_ethn) * factor(mrp_age) +
                              factor(mrp_ethn) * factor(mrp_income_cat) +
                              (1 | mrp_ethn) + (1 | mrp_gender) + (1 | mrp_pid) + (1 | mrp_age) + 
                              (1 | mrp_income_cat),
                            data = df_mrp, prior = normal(-1, 1), family = binomial(link = "logit"),
                            cores = 4)
# post stratification table
source("code/States/NC/clean_absentee_nc_2018.R")
df_2018 <- df
df_post <- df_2018 %>%
  mutate(mrp_ethn = ifelse(ethn == "native" | ethn == "pac" | ethn == "asian", "other", ethn),
         mrp_age = ifelse(age < 25, "Below 25", 
                          ifelse(age < 40, "Below 40",
                                 ifelse(age < 55, "Below 55", "Above equal 55"))),
         mrp_gender = gender,
         mrp_pid    = ifelse(voter_party_code %in% c("DEM", "REP"), voter_party_code, "Other"),
         mrp_income_cat = ifelse(zip_median_income <= 40000, "Below 40000", 
                                 ifelse(zip_median_income > 40000 & zip_median_income <= 60000, "Bet 40 and 50",
                                        ifelse(zip_median_income > 60000, "Above 60000", 4))),
         rejected = ifelse(ballot_rtn_status != "RETURNED UNDELIVERABLE" & 
                           ballot_rtn_status != "PENDING" &
                           ballot_rtn_status != "SPOILED" &
                           ballot_req_type == "MAIL" &
                           ballot_rtn_status != "" &
                           ballot_rtn_status != "ACCEPTED", 1, 0),
         by_mail = ifelse(ballot_rtn_status != "RETURNED UNDELIVERABLE" & 
                            ballot_rtn_status != "PENDING" &
                            ballot_rtn_status != "SPOILED" &
                            ballot_req_type == "MAIL" &
                            ballot_rtn_status != "", 1, 0)
  ) %>%
  filter(mrp_gender != "U") %>%
  group_by(mrp_ethn, mrp_age, mrp_gender, mrp_pid, mrp_income_cat) %>%
  summarize(N = n(),
            N_rejected = sum(rejected),
            N_mail = sum(by_mail))
# prediction (MRP)
posterior_prob <- posterior_linpred(fit, transform = TRUE, newdata = df_post)
poststrat_prob <- posterior_prob %*% df_post$N / sum(df_post$N)
model_popn_pref <- c(mean = mean(poststrat_prob), sd = sd(poststrat_prob))
round(model_popn_pref, 3)
# prediction sample
mean_sample_popn_pref <- 1 - mean(df_2020$status)
sd_sample_popn_pref <- sqrt(mean_sample_popn_pref * (1 - mean_sample_popn_pref)/nrow(df_2020))
round(c(sample_popn_pref, sd_sample_popn_pref), 5)


ethn_levels <- levels(as.factor(df_mrp$mrp_ethn))
ethn_df <- data.frame(
  ethn = ethn_levels,
  model_ethn_sd = rep(-1, length(ethn_levels)),
  model_ethn_pref = rep(-1, length(ethn_levels)),
  sample_ethn_pref = rep(-1, length(ethn_levels)),
  N = rep(-1, length(ethn_levels)),
  model_N_rejected_mean_ethn = rep(-1, length(ethn_levels)),
  model_N_rejected_sd_ethn = rep(-1, length(ethn_levels)),
  true_N_cast_ethn = rep(-1, length(ethn_levels)),
  true_N_mail_ethn = rep(-1, length(ethn_levels)),
  true_N_rejected_ethn = rep(-1, length(ethn_levels))
)

for(i in 1:length(levels(as.factor(df_mrp$mrp_ethn)))) {
  poststrat_ethn <- df_post[df_post$mrp_ethn == ethn_levels[i], ]
  posterior_prob_state <- posterior_linpred(
    fit,
    transform = TRUE,
    draws = 1000,
    newdata = as.data.frame(poststrat_ethn)
  )
  poststrat_prob_ethn <- (posterior_prob_state %*% poststrat_ethn$N) / sum(poststrat_ethn$N)
  # Expected N of rejected and N cast
  ethn_df$model_N_rejected_mean_ethn[i] <- mean(posterior_prob_state %*% poststrat_ethn$N)
  ethn_df$model_N_rejected_sd_ethn[i] <- sd(posterior_prob_state %*% poststrat_ethn$N)
  ethn_df$true_N_cast_ethn[i] <- sum(poststrat_ethn$N)
  ethn_df$true_N_rejected_ethn[i] <- sum(poststrat_ethn$N_rejected)
  ethn_df$true_N_mail_ethn[i] <- sum(poststrat_ethn$N_mail)
  #This is the estimate for popn in state:
  ethn_df$model_ethn_pref[i] <- round(mean(poststrat_prob_ethn), 4)
  ethn_df$model_ethn_sd[i] <- round(sd(poststrat_prob_ethn), 4)
  #This is the estimate for sample
  ethn_df$sample_ethn_pref[i] <- round(1 - mean(df_2020$status[df_2020$ethn == ethn_levels[i]]), 4)
  ethn_df$N[i] <- length(df_2020$status[df_2020$ethn == ethn_levels[i]])
}
# get rates
df_model <- cbind(ethn_df[, c("ethn", "model_N_rejected_mean_ethn", "true_N_cast_ethn", "model_N_rejected_sd_ethn")], rep("2018 (MRP all)", 4))
colnames(df_model) <- c("ethn", "rejected", "true_cast", "rejected_sd", "kind")
df_true  <- cbind(ethn_df[, c("ethn", "true_N_rejected_ethn", "true_N_mail_ethn")], 0, rep("2018 (true)", 4))
colnames(df_true) <- c("ethn", "rejected", "true_cast", "rejected_sd", "kind")
df_joint_2018 <- rbind(df_model,df_true) %>%
  group_by(kind) %>%
  mutate(rejected_share  = rejected/sum(rejected),
         submitted_share = true_cast/sum(true_cast),
         upper_rejected  = (rejected + 1.96 * rejected_sd)/sum(rejected + 1.96 * rejected_sd),
         lower_rejected  = (rejected - 1.96 * rejected_sd)/sum(rejected - 1.96 * rejected_sd))
plt <- ggplot(data = df_joint_2018, aes(x = submitted_share, y = rejected_share, color = kind)) + 
  geom_point() + 
  geom_errorbar(aes(x = submitted_share, y = rejected_share, 
                    ymin = lower_rejected, ymax = upper_rejected, color = kind)) +
  geom_text(aes(label = ethn), color = "black", vjust = "inward") +
  geom_abline() + 
  labs(x = "Share among submitted", y = "Share among rejected") + 
  theme_bw() + 
  lims(x = c(0, 0.8), y = c(0, 0.8))
ggsave("plots/States/NC/MRP_share_submitted_rejected_2018.jpeg", plt, width = 9, height = 6)
plt <- ggplot(data = df_joint_2018 %>%
         mutate(mrp = ifelse(grepl("MRP", kind), "all (MRP 2020)", "true")), 
       aes(x = ethn, y = rejected, color = mrp)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  labs(y = "Number rejected", color = "Kind",
       caption = "True refers to actual absentee voters
       MRP refers to the case had everyone voted absentee",
       title = "Number rejected for 2018")
ggsave("plots/States/NC/MRP_N_submitted_rejected_2018.jpeg", plt, width = 9, height = 6)


# post stratification table
source("code/States/NC/clean_absentee_nc_2016.R")
df_2016 <- df
df_post <- df_2016 %>%
  mutate(mrp_ethn = ifelse(ethn == "native" | ethn == "pac" | ethn == "asian", "other", ethn),
         mrp_age = ifelse(age < 25, "Below 25", 
                          ifelse(age < 40, "Below 40",
                                 ifelse(age < 55, "Below 55", "Above equal 55"))),
         mrp_gender = gender,
         mrp_pid    = ifelse(voter_party_code %in% c("DEM", "REP"), voter_party_code, "Other"),
         mrp_income_cat = ifelse(zip_median_income <= 40000, "Below 40000", 
                                 ifelse(zip_median_income > 40000 & zip_median_income <= 60000, "Bet 40 and 50",
                                        ifelse(zip_median_income > 60000, "Above 60000", 4))),
         rejected = ifelse(ballot_rtn_status != "RETURNED UNDELIVERABLE" & 
                             ballot_rtn_status != "PENDING" &
                             ballot_rtn_status != "SPOILED" &
                             ballot_req_type == "MAIL" &
                             ballot_rtn_status != "" &
                             ballot_rtn_status != "ACCEPTED", 1, 0),
         by_mail = ifelse(ballot_rtn_status != "RETURNED UNDELIVERABLE" & 
                            ballot_rtn_status != "PENDING" &
                            ballot_rtn_status != "SPOILED" &
                            ballot_req_type == "MAIL" &
                            ballot_rtn_status != "", 1, 0)
  ) %>%
  filter(mrp_gender != "U") %>%
  group_by(mrp_ethn, mrp_age, mrp_gender, mrp_pid, mrp_income_cat) %>%
  summarize(N = n(),
            N_rejected = sum(rejected, na.rm = TRUE),
            N_mail = sum(by_mail, na.rm = TRUE))
# prediction (MRP)
posterior_prob <- posterior_linpred(fit, transform = TRUE, newdata = df_post)
poststrat_prob <- posterior_prob %*% df_post$N / sum(df_post$N)
model_popn_pref <- c(mean = mean(poststrat_prob), sd = sd(poststrat_prob))
round(model_popn_pref, 3)
# prediction sample
mean_sample_popn_pref <- 1 - mean(df_2020$status)
sd_sample_popn_pref <- sqrt(mean_sample_popn_pref * (1 - mean_sample_popn_pref)/nrow(df_2020))
round(c(sample_popn_pref, sd_sample_popn_pref), 5)


ethn_levels <- levels(as.factor(df_mrp$mrp_ethn))
ethn_df <- data.frame(
  ethn = ethn_levels,
  model_ethn_sd = rep(-1, length(ethn_levels)),
  model_ethn_pref = rep(-1, length(ethn_levels)),
  sample_ethn_pref = rep(-1, length(ethn_levels)),
  N = rep(-1, length(ethn_levels)),
  model_N_rejected_mean_ethn = rep(-1, length(ethn_levels)),
  model_N_rejected_sd_ethn = rep(-1, length(ethn_levels)),
  true_N_cast_ethn = rep(-1, length(ethn_levels)),
  true_N_mail_ethn = rep(-1, length(ethn_levels)),
  true_N_rejected_ethn = rep(-1, length(ethn_levels))
)

for(i in 1:length(levels(as.factor(df_mrp$mrp_ethn)))) {
  poststrat_ethn <- df_post[df_post$mrp_ethn == ethn_levels[i], ]
  posterior_prob_state <- posterior_linpred(
    fit,
    transform = TRUE,
    draws = 1000,
    newdata = as.data.frame(poststrat_ethn)
  )
  poststrat_prob_ethn <- (posterior_prob_state %*% poststrat_ethn$N) / sum(poststrat_ethn$N)
  # Expected N of rejected and N cast
  ethn_df$model_N_rejected_mean_ethn[i] <- mean(posterior_prob_state %*% poststrat_ethn$N)
  ethn_df$model_N_rejected_sd_ethn[i] <- sd(posterior_prob_state %*% poststrat_ethn$N)
  ethn_df$true_N_cast_ethn[i] <- sum(poststrat_ethn$N)
  ethn_df$true_N_rejected_ethn[i] <- sum(poststrat_ethn$N_rejected)
  ethn_df$true_N_mail_ethn[i] <- sum(poststrat_ethn$N_mail)
  #This is the estimate for popn in state:
  ethn_df$model_ethn_pref[i] <- round(mean(poststrat_prob_ethn), 4)
  ethn_df$model_ethn_sd[i] <- round(sd(poststrat_prob_ethn), 4)
  #This is the estimate for sample
  ethn_df$sample_ethn_pref[i] <- round(1 - mean(df_2020$status[df_2020$ethn == ethn_levels[i]]), 4)
  ethn_df$N[i] <- length(df_2020$status[df_2020$ethn == ethn_levels[i]])
}
# get rates
df_model <- cbind(ethn_df[, c("ethn", "model_N_rejected_mean_ethn", "true_N_cast_ethn", "model_N_rejected_sd_ethn")], rep("2016 (MRP all)", 4))
colnames(df_model) <- c("ethn", "rejected", "true_cast", "rejected_sd", "kind")
df_true  <- cbind(ethn_df[, c("ethn", "true_N_rejected_ethn", "true_N_mail_ethn")], 0, rep("2016 (true)", 4))
colnames(df_true) <- c("ethn", "rejected", "true_cast", "rejected_sd", "kind")
df_joint_2016 <- rbind(df_model,df_true) %>%
  group_by(kind) %>%
  mutate(rejected_share  = rejected/sum(rejected),
         submitted_share = true_cast/sum(true_cast),
         upper_rejected  = (rejected + 1.96 * rejected_sd)/sum(rejected + 1.96 * rejected_sd),
         lower_rejected  = (rejected - 1.96 * rejected_sd)/sum(rejected - 1.96 * rejected_sd))
plt <- ggplot(data = df_joint_2016, aes(x = submitted_share, y = rejected_share, color = kind)) + 
  geom_point() + 
  geom_errorbar(aes(x = submitted_share, y = rejected_share, 
                    ymin = lower_rejected, ymax = upper_rejected, color = kind)) +
  geom_text(aes(label = ethn), color = "black", vjust = "inward") +
  geom_abline() + 
  labs(x = "Share among submitted", y = "Share among rejected") + 
  theme_bw() + 
  lims(x = c(0, 0.9), y = c(0, 0.9))
ggsave("plots/States/NC/MRP_share_submitted_rejected_2016.jpeg", plt, width = 9, height = 6)
plt <- ggplot(data = df_joint_2016 %>%
                mutate(mrp = ifelse(grepl("MRP", kind), "all (MRP 2020)", "true")), 
              aes(x = ethn, y = rejected, color = mrp)) + 
  geom_point() + 
  theme_bw() + 
  theme(axis.title.x = element_blank()) + 
  labs(y = "Number rejected", color = "Kind",
       caption = "True refers to actual absentee voters
       MRP refers to the case had everyone voted absentee",
       title = "Number rejected for 2016")
ggsave("plots/States/NC/MRP_N_submitted_rejected_2016.jpeg", plt, width = 9, height = 6)

# 2016 / 2018
df_joint <- rbind(df_joint_2016 %>% add_column(year = 2016),
                  df_joint_2018 %>% add_column(year = 2018))

plt <- ggplot(data = df_joint %>% 
         mutate(kind = factor(kind, levels = c("2016 (true)", "2016 (MRP all)", "2018 (true)", "2018 (MRP all)")),
                mrp = ifelse(grepl("MRP", kind), "MRP", "true")),
       aes(x = factor(year), rejected_share, color = mrp)) + 
  geom_point() +
  geom_errorbar(aes(x = factor(year), y = rejected_share, 
                    ymin = lower_rejected, ymax = upper_rejected, color = mrp), width = 0) + 
  facet_grid(.~ethn, space = "free_x") +
  theme_bw() +
  theme(axis.title.x = element_blank()) + 
  labs(color = "Kind", y = "Share of rejected absentee ballots",
       caption = "True refers to actual absentee voters
       MRP refers to the case had everyone voted absentee")
ggsave("plots/States/NC/MRP_share_rejected_2016_2018.jpeg", plt, width = 9, height = 6)

  
  