# VBM
# all VBM Oregon, Washington, Colorado, Utah, Hawaii
# improve by not including these states when estimating request rates because they would get infalted in other states
# Oregon does not offer in person voting
# Washington offers in person and vote by mail
# Colorado offers in person and vote by mail
# Utah offers in person and vote by mail
# Hawaii offers in person and vote by mail



source("code/load_model_data.R")
source("code/functions.R")
# descriptive viz
req_sub <- ggplot(data = df_subset, aes(x = pr1, y = pr2)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = df_subset, aes(x = pr2, y = pr3)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = df_subset, aes(x = pr1, y = pr3)) + 
  geom_point(size = 0.2)
grid.arrange(req_sub, sub_rej, req_rej)

# data
# drop states with only one observation
# create group id
# create a crosswalk from state names to group ids
df_subset  <- df_subset %>% 
  group_by(State) %>%
  filter(n() > 2) %>%
  ungroup()

df_subset_xbar1 <- df_subset %>%
  filter(!(State %in% c("OR", "WA", "CO", "UT", "HI"))) %>%
  mutate(group_id = group_indices(., State))
df_subset_xbar2 <- df_subset %>% 
  filter(State %in% c("WA", "CO", "UT", "HI")) %>%
    mutate(group_id = max(df_subset_xbar1$group_id) + group_indices(., State))
df_subset_xbar3 <- df_subset %>% 
  filter(State %in% c("OR")) %>%
  mutate(group_id = max(df_subset_xbar2$group_id) + group_indices(., State))

region_crosswalk <- rbind(
  df_subset_xbar1 %>% distinct(State, group_id),
  df_subset_xbar2 %>% distinct(State, group_id),
  df_subset_xbar3 %>% distinct(State, group_id)
) %>% arrange(group_id)

write_rds(df_subset_xbar1, path = "model_fits/fit_model_state_level_turnout_allVBM_xbar1.Rds")
write_rds(df_subset_xbar2, path = "model_fits/fit_model_state_level_turnout_allVBM_xbar2.Rds")
write_rds(df_subset_xbar3, path = "model_fits/fit_model_state_level_turnout_allVBM_xbar3.Rds")

# model
model <- rstan::stan_model("code/stan/v11_ecological_regression_prop_w_allVBM.stan")
data_model <- list(
  J = nrow(df_subset),
  G = 5,
  S = length(unique(df_subset$group_id)),
  s = df_subset$group_id,
  xbar1 = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")],
  ybar = df_subset[,c("pr1", "pr2", "pr3")]
)
fit <- rstan::sampling(model, data_model, chains = 4, cores = 4, warmup = 1500, iter = 2250)
write_rds(fit, path = "model_fits/fit_m11_states.Rds")


# prediction
# create posterior compositions of the groups of those who requested and submitted absentee ballots
fit <- read_rds("model_fits/fit_m11_states.Rds")
df_subset <- read_rds('model_fits/fit_m11_states_df_subset.Rds')
nsim = 100
ybar <- array(NA, dim = c(data_model$J, 3, nsim))
beta_region <- rstan::extract(fit, pars = "beta")[[1]]
sigma <- rstan::extract(fit, pars = "sigma")[[1]]
counter = 0
# for (i in sample(1:3000, nsim,replace = FALSE)){
#   counter = counter + 1
#   xbar2 <- beta_region[i, 1, data_model$s,] * data_model$xbar1
#   xbar2 <- xbar2/rowSums(xbar2)
#   xbar3 <- beta_region[i, 2, data_model$s,] * xbar2
#   xbar3 <- xbar3/rowSums(xbar3)
#   ybar[,1, counter] <- rnorm_truncated(rowSums(beta_region[i, 1, data_model$s,] * data_model$xbar1), sigma[i, 1], 0, 1)
#   ybar[,2, counter] <- rnorm_truncated(rowSums(beta_region[i, 2, data_model$s,] * xbar2), sigma[i, 2], 0, 1)
#   ybar[,3, counter] <- rnorm_truncated(rowSums(beta_region[i, 3, data_model$s,] * xbar3), sigma[i, 3], 0, 1)
# }
for (i in sample(1:3000, nsim,replace = FALSE)){
  counter = counter + 1
  xbar2 <- beta_region[i, 1, data_model$s,] * data_model$xbar1
  xbar2 <- xbar2/rowSums(xbar2)
  xbar3 <- beta_region[i, 2, data_model$s,] * xbar2
  xbar3 <- xbar3/rowSums(xbar3)
  ybar[,1, counter] <- rnorm(data_model$J, rowSums(beta_region[i, 1, data_model$s,] * data_model$xbar1), sigma[i, 1])
  ybar[,2, counter] <- rnorm(data_model$J, rowSums(beta_region[i, 2, data_model$s,] * xbar2), sigma[i, 2])
  ybar[,3, counter] <- rnorm(data_model$J, rowSums(beta_region[i, 3, data_model$s,] * xbar3), sigma[i, 3])
}
ybar[ybar > 1] <- 1
ybar[ybar < 0] <- 0
# overlap
# national
coverage_025_975_requested <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 1] > quantile(ybar[x, 1,], 0.025)) & (data_model$ybar[x, 1] < quantile(ybar[x, 1,], 0.975)) ))
coverage_025_975_submitted <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 2] > quantile(ybar[x, 2,], 0.025)) & (data_model$ybar[x, 2] < quantile(ybar[x, 2,], 0.975)) ))
coverage_025_975_rejected <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 3] > quantile(ybar[x, 3,], 0.025)) & (data_model$ybar[x, 3] < quantile(ybar[x, 3,], 0.975)) ))
cat("95% interval coverage for requested:", round(mean(coverage_025_975_requested), 4), "\n",
    "95% interval coverage for submitted:", round(mean(coverage_025_975_submitted), 4), "\n",
    "95% interval coverage for rejected:", round(mean(coverage_025_975_rejected), 4))
coverage_25_75_requested <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 1] > quantile(ybar[x, 1,], 0.25)) & (data_model$ybar[x, 1] < quantile(ybar[x, 1,], 0.75)) ))
coverage_25_75_submitted <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 2] > quantile(ybar[x, 2,], 0.25)) & (data_model$ybar[x, 2] < quantile(ybar[x, 2,], 0.75)) ))
coverage_25_75_rejected <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 3] > quantile(ybar[x, 3,], 0.25)) & (data_model$ybar[x, 3] < quantile(ybar[x, 3,], 0.75)) ))
cat("50% interval coverage for requested:", round(mean(coverage_25_75_requested), 4), "\n",
    "50% interval coverage for submitted:", round(mean(coverage_25_75_submitted), 4), "\n",
    "50% interval coverage for rejected:", round(mean(coverage_25_75_rejected), 4))
# state-level
# compute interval coverage on the state level
coverage <- data.frame(State = region_crosswalk[as.integer(data_model$s),"State"] %>% pull(State),
                       requested = coverage_025_975_requested,
                       submitted = coverage_025_975_submitted,
                       rejected = coverage_025_975_rejected)
coverage <- coverage %>% group_by(State) %>%
  summarize_all(list(mean = mean)) %>%
  pivot_longer(cols = c(requested_mean, submitted_mean, rejected_mean), 
               names_to = "kind", names_pattern = "(.+)_.+", values_to = "mean")
ggplot(coverage %>% mutate(State = factor(State, levels = coverage %>%
                                            filter(kind == "requested") %>%
                                            arrange(mean) %>%
                                            pull(State))), aes(x = State, y = mean, color = kind)) + 
  geom_point(position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.95) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + 
  labs(y = "95% interval coverage")
coverage <- data.frame(State = region_crosswalk[as.integer(data_model$s),"State"] %>% pull(State),
                       requested = coverage_25_75_requested,
                       submitted = coverage_25_75_submitted,
                       rejected = coverage_25_75_rejected)
coverage <- coverage %>% group_by(State) %>%
  summarize_all(list(mean = mean)) %>%
  pivot_longer(cols = c(requested_mean, submitted_mean, rejected_mean), 
               names_to = "kind", names_pattern = "(.+)_.+", values_to = "mean")
ggplot(coverage %>% mutate(State = factor(State, levels = coverage %>%
                                            filter(kind == "requested") %>%
                                            arrange(mean) %>%
                                            pull(State))), aes(x = State, y = mean, color = kind)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0.5) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + 
  labs(y = "50% interval coverage")

# predicted values
## requested
ybar_requested <- as.data.frame(cbind(data_model$s, ybar[,1,]))
names(ybar_requested)[1] <- "group_id"
ybar_requested <- ybar_requested %>% pivot_longer(cols = c(-group_id), values_to = "y", names_to = "sim", names_prefix = "V")
ybar_requested$State <- region_crosswalk[ybar_requested$group_id,] %>% pull(State)
ybar_requested <- ybar_requested %>% dplyr::select(y, State) %>% mutate(kind = "predicted")
ybar_requested_true <- data.frame(y = df_subset$pr1, State = df_subset$State, kind = "true")
ybar_requested <- bind_rows(ybar_requested, ybar_requested_true)
ppd_requested <- ggplot(data = ybar_requested, aes(x = y, fill = kind)) + 
  geom_histogram(stat = "density", position = "identity", alpha = 0.5) + 
  facet_wrap(State ~ ., scales = "free_y") + 
  labs(title = "Requested PPD") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_requested_by_state.jpeg"), ppd_requested)
## submitted
ybar_submitted <- as.data.frame(cbind(data_model$s, ybar[,2,]))
names(ybar_submitted)[1] <- "group_id"
ybar_submitted <- ybar_submitted %>% 
  pivot_longer(cols = c(-group_id), values_to = "y", names_to = "sim", names_prefix = "V")
ybar_submitted$State <- region_crosswalk[ybar_submitted$group_id,] %>% pull(State)
ybar_submitted <- ybar_submitted %>% dplyr::select(y, State) %>% mutate(kind = "predicted")
ybar_submitted_true <- data.frame(y = df_subset$pr2, State = df_subset$State, kind = "true")
ybar_submitted <- bind_rows(ybar_submitted, ybar_submitted_true)
ppd_submitted <- ggplot(data = ybar_submitted, aes(x = y, fill = kind)) + 
  geom_histogram(stat = "density", position = "identity", alpha = 0.5) + 
  facet_wrap(State ~ ., scales = "free_y") + 
  labs(title = "Submitted PPD") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave(paste0("plots/m11_", Sys.Date(), "_ppd_submitted_by_state.jpeg"), ppd_submitted)
## rejected
ybar_rejected <- as.data.frame(cbind(data_model$s, ybar[,3,]))
names(ybar_rejected)[1] <- "group_id"
ybar_rejected <- ybar_rejected %>% 
  pivot_longer(cols = c(-group_id), values_to = "y", names_to = "sim", names_prefix = "V")
ybar_rejected$State <- region_crosswalk[ybar_rejected$group_id,] %>% pull(State)
ybar_rejected <- ybar_rejected %>% dplyr::select(y, State) %>% mutate(kind = "predicted")
ybar_rejected_true <- data.frame(y = df_subset$pr3, State = df_subset$State, kind = "true")
ybar_rejected <- bind_rows(ybar_rejected, ybar_rejected_true)
ppd_rejected <- ggplot(data = ybar_rejected, aes(x = y, fill = kind)) + 
  geom_histogram(stat = "density", position = "identity", alpha = 0.5) + 
  facet_wrap(State ~ ., scales = "free_y") + 
  labs(title = "Rejected PPD") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_rejected_by_state.jpeg"), ppd_rejected)
ppd_requested
ppd_submitted
ppd_rejected

# minimum
minimum_predicted <- as.data.frame(t(apply(ybar, MARGIN = c(2,3), min)))
colnames(minimum_predicted) <- c("requested", "submitted", "rejected")
minimum_predicted %>% pivot_longer(cols = everything(), names_to = "kind", values_to = "ybar") %>%
  ggplot(.) + 
  geom_histogram(aes(x = ybar)) + 
  facet_wrap(kind ~ ., scales = "free") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  labs(title = "Predicted minimum")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_minimum.jpeg"), minimum_predicted)
maximum_predicted <- as.data.frame(t(apply(ybar, MARGIN = c(2,3), max)))
colnames(maximum_predicted) <- c("requested", "submitted", "rejected")
maximum_predicted %>% pivot_longer(cols = everything(), names_to = "kind", values_to = "ybar") %>%
  ggplot(.) + 
  geom_histogram(aes(x = ybar)) + 
  facet_wrap(kind ~ ., scales = "free") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
  labs(title = "Predicted maximum")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_maximum.jpeg"), maximum_predicted)


# residuals
counties <- rgdal::readOGR(dsn = 
                            "data/shapefiles", 
                          layer = "cb_2019_us_county_20m")
ybar_mean <- apply(ybar, MARGIN = c(1, 2), mean)
df_residuals <- data.frame(FIPSCode = df_subset$FIPSCode, 
                           y1 = df_subset$pr1,
                           y2 = df_subset$pr2,
                           y3 = df_subset$pr3,
                           y1_hat = ybar_mean[, 1],
                           y2_hat = ybar_mean[, 2],
                           y3_hat = ybar_mean[, 3]
) %>% 
  mutate(
    residuals1 = y1 - y1_hat,
    residuals2 = y2 - y2_hat,
    residuals3 = y3 - y3_hat
  )
counties@data <- counties@data %>% 
  mutate(FIPSCode = as.integer(paste0(STATEFP, COUNTYFP))) %>%
  left_join(df_residuals, by = "FIPSCode") %>%
  left_join(df_subset, by = "FIPSCode")
# plotting
counties@data$id <- rownames(counties@data)
counties_points <- fortify(counties, region = "id")
counties_df <- merge(counties_points, counties@data, by = "id")
res1 <- ggplot(counties_df %>% filter(State != "HI", State != "AK"), aes(long, lat, fill = residuals1)) +
  geom_polygon(aes(group = group)) +
  facet_wrap(~State, scales = "free")
res2 <- ggplot(counties_df %>% filter(State != "HI", State != "AK"), aes(long, lat, fill = residuals2)) +
  geom_polygon(aes(group = group)) +
  facet_wrap(~State, scales = "free")
res3 <- ggplot(counties_df %>% filter(State != "HI", State != "AK"), aes(long, lat, fill = residuals3)) +
  geom_polygon(aes(group = group)) +
  facet_wrap(~State, scales = "free")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_residuals_requested.jpeg"), res1)
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_residuals_submitted.jpeg"), res2)
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_residuals_rejected.jpeg"), res3)

# coefplot
cat <- df_subset %>% dplyr::select(group_id, State) %>% distinct()
groups <- c("white", "black", "latinx", 'asian', "other")
kind <- c("requested", "submitted", "rejected")
beta_region <- rstan::extract(fit, pars = "beta")[[1]]
# [iter, kind, region, group]
df_region_coef <- matrix(NA, nrow = 0, ncol = 6)
for (i in 1:nrow(cat)){
  for (j in 1:4){
    for (r in 1:3){
      add <- cbind(cat[cat$group_id == i,"State"],
                   groups[j], 
                   kind[r], 
                   mean(beta_region[,r,i,j]), 
                   quantile(beta_region[,r,i,j], c(0.25)), 
                   quantile(beta_region[,r,i,j], c(0.75)))
      df_region_coef <- rbind(df_region_coef, add)
    }
  }
}
df_region_coef <- as.data.frame(df_region_coef)
colnames(df_region_coef) <- c("State", "group", "kind", "mean_rate", "q025", "q075")
rownames(df_region_coef) <- NULL
df_region_coef <- df_region_coef %>%
  mutate(
    mean_rate = as.numeric(as.character(mean_rate)),
    q025 = as.numeric(as.character(q025)),
    q075 = as.numeric(as.character(q075)),
    kind = factor(kind, levels = c("requested", "submitted", "rejected"))
  )
df_mean_rates_region <- df_subset %>%
  group_by(State) %>%
  summarize(
    requested = mean(pr1, na.rm = TRUE),
    submitted = mean(pr2, na.rm = TRUE),
    rejected = mean(pr3, na.rm = TRUE)
  ) %>% pivot_longer(cols = c(requested, submitted, rejected), names_to = "kind", values_to = "mean_rate") %>% 
  add_column(q025 = NA, q075 = NA, group = "average") %>%
  dplyr::select(State, group, kind, mean_rate, q025, q075)
df_region_coef <- rbind(df_region_coef, df_mean_rates_region)
coef_plot_region <- ggplot(data = df_region_coef, aes(x = State, y = mean_rate, color = group, shape = group)) +
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, y = mean_rate, ymin = q025, ymax = q075, color = group, shape = group), 
                position = position_dodge(0.5), width = 0) +
  scale_shape_manual(values=c(16, 16, 16,16,6)) + 
  scale_colour_brewer(palette = palette) + 
  facet_wrap(.~ kind, scales = "free_y", ncol = 1) + 
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

palette <- "Set1"
coef_plot_region_requested <- ggplot(data = df_region_coef %>% 
                                       filter(kind == "requested") %>%
                                       mutate(State = factor(State, levels = df_region_coef %>% 
                                                filter(kind == 'rejected', group == "average") %>% 
                                                arrange(mean_rate) %>% 
                                                pull(State))), aes(x = State, y = mean_rate, color = group, shape = group)) +
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, y = mean_rate, ymin = q025, ymax = q075, color = group, shape = group), 
                position = position_dodge(0.5), width = 0) +
  scale_shape_manual(values=c(16, 16, 16,16,6)) + 
  scale_colour_brewer(palette = palette) + 
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) +
  labs(y = "Requested")
coef_plot_region_submitted <- ggplot(data = df_region_coef %>% 
                                       filter(kind == "submitted") %>%
                                       mutate(State = factor(State, levels = df_region_coef %>% 
                                                               filter(kind == 'rejected', group == "average") %>% 
                                                               arrange(mean_rate) %>% 
                                                               pull(State))), aes(x = State, y = mean_rate, color = group, shape = group)) +
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, y = mean_rate, ymin = q025, ymax = q075, color = group, shape = group), 
                position = position_dodge(0.5), width = 0) +
  scale_shape_manual(values=c(16, 16, 16,16,6)) + 
  scale_colour_brewer(palette = palette) + 
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) +
  labs(y = "Submitted")
coef_plot_region_rejected <- ggplot(data = df_region_coef %>% 
                                       filter(kind == "rejected") %>%
                                       mutate(State = factor(State, levels = df_region_coef %>% 
                                                               filter(kind == 'rejected', group == "average") %>% 
                                                               arrange(mean_rate) %>% 
                                                               pull(State))), aes(x = State, y = mean_rate, color = group, shape = group)) +
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, y = mean_rate, ymin = q025, ymax = q075, color = group, shape = group), 
                position = position_dodge(0.5), width = 0) +
  scale_shape_manual(values=c(16, 16, 16,16,6)) + 
  scale_colour_brewer(palette = palette) + 
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90), axis.title.x = element_blank(), legend.title = element_blank()) + 
  labs(y = "Rejected")
plt_grid <- grid.arrange(coef_plot_region_requested, coef_plot_region_submitted, coef_plot_region_rejected, ncol = 1)
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_pr_with_turnout_by_state.jpeg"), plt_grid)



# voting power 
beta_region <- rstan::extract(fit, pars = "beta")[[1]]
states <- df_subset %>% distinct(State) %>% pull(State)
n_states <- length(states)
df_subset <- df_subset %>%
  mutate(voters_count = total_votes_2016)
# voters
# FIPSCode, State, n_absentee_voters, white, black, latino, other,
#                  n_submitted_voters, white, black, latino,other
#                  n_rejected_voters, white, black, latino, other
df_sim <- matrix(NA, nrow = 0, ncol = 25)
count = 0
for (id in sample(1:3000, 6, replace = FALSE)){
  count = count + 1
  print(count/6)
  for (i in 1:nrow(df_subset)) {
    n_absentee_voters = 0
    n_absentee_group = rep(0, 5)
    n_voters = df_subset[i, "voters_count"]
    i_region = as.integer(df_subset[i, "group_id"])
    n_limit = floor(n_voters * .7)
    while (n_absentee_voters < n_limit) {
      n_group <- df_subset[i, c("voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other")] - n_absentee_group
      val = sapply(1:5, function(x) rbinom(1, as.integer(n_group[x]), prob = as.numeric(beta_region[id, 1, i_region, x])))
      n_absentee_group = n_absentee_group + val
      n_absentee_voters = sum(n_absentee_group)
    }
    n_submitting_voters = 0
    n_submitting_group = rep(0, 4)
    n_submitting_group = sapply(1:5, function(x) rbinom(1, 
                                                        n_absentee_group[x], 
                                                        prob = as.numeric(beta_region[id, 2, i_region, x])))
    n_submitting_voters = sum(n_submitting_group)
    n_rejected_voters = 0
    n_rejected_group = rep(0, 5)
    n_rejected_group = sapply(1:5, function(x) rbinom(1, 
                                                      n_submitting_group[x], 
                                                      prob = as.numeric(beta_region[id, 3, i_region, x])))
    n_rejected_voters = sum(n_rejected_group)
    data <- df_subset[i, c("FIPSCode", "group_id", "voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other")]
    add <-  c(n_absentee_voters, n_absentee_group, 
              n_submitting_voters, n_submitting_group,
              n_rejected_voters, n_rejected_group)
    df_sim <- rbind(df_sim, c(as.character(data), add))
  }
}
df_sim <- as.data.frame(df_sim)
df_sim <- turn_numeric(df_sim, -1)
region_crosswalk <- df_subset %>% 
  dplyr::select(State, group_id) %>% 
  distinct() %>%
  arrange(group_id)
colnames(df_sim) <- c("FIPSCode", "State", "voters_white", "voters_black", "voters_hispanic", "voters_asian", "voters_other",
                      "n_requested", "n_requested_white", "n_requested_black", "n_requested_hispanic", "n_requested_asian", "n_requested_other",
                      "n_submitted", "n_submitted_white", "n_submitted_black", "n_submitted_hispanic", "n_submitted_asian", "n_submitted_other",
                      "n_rejected", "n_rejected_white", "n_rejected_black", "n_rejected_hispanic", "n_rejected_asian", "n_rejected_other")
df_sim$State <- region_crosswalk[as.integer(df_sim$State),"State"] %>% pull(State)
write_rds(df_sim, path = "model_fits/fit_simulated_70percent_turnout_by_state.Rds")
# simulation plots
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup() %>% 
  group_by(State) %>%
  summarize_all(list(mean = mean, sd = sd))
## black ----
plt_n_black_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_black_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_black_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_black_sum_mean + n_rejected_black_sum_sd,
                    ymin = n_rejected_black_sum_mean - n_rejected_black_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Black absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_n_rejected_black_by_state_turnout_by_state.jpeg"), plt_n_black_rejected)
## hispanic ----
plt_n_latinx_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_hispanic_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_hispanic_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_hispanic_sum_mean + n_rejected_hispanic_sum_sd,
                    ymin = n_rejected_hispanic_sum_mean - n_rejected_hispanic_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Latinx absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_n_rejected_latinx_by_state_turnout_by_state.jpeg"), plt_n_latinx_rejected)
## asian ----
plt_n_asian_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_asian_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_asian_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_asian_sum_mean + n_rejected_asian_sum_sd,
                    ymin = n_rejected_asian_sum_mean - n_rejected_asian_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Asian absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_n_asian_latinx_by_state_turnout_by_state.jpeg"), plt_n_asian_rejected)
## white ----
require(scales)
plt_n_white_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_white_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_white_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_white_sum_mean + n_rejected_white_sum_sd,
                    ymin = n_rejected_white_sum_mean - n_rejected_white_sum_sd), width = 0) +
  scale_y_continuous(labels = comma) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected white absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_n_rejected_white_by_state_turnout_by_state.jpeg"), plt_n_white_rejected)
## nation ----
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(sim) %>% dplyr::select(-State) %>%
  summarise_all(list(sum = sum)) %>% 
  ungroup() %>% 
  summarize_all(list(mean = mean, sd = sd))

rejected_mean <- df_sim %>% pivot_longer(cols = c(n_rejected_white_sum_mean, n_rejected_black_sum_mean,
                                                  n_rejected_hispanic_sum_mean, n_rejected_asian_sum_mean, n_rejected_other_sum_mean),
                                         names_pattern = "n_(.+)_sum_mean",
                                         names_to = "kind", values_to = "mean") %>%
  dplyr::select(kind, mean)
rejected_sd <- df_sim %>% pivot_longer(cols = c(n_rejected_white_sum_sd, n_rejected_black_sum_sd,
                                                n_rejected_hispanic_sum_sd, n_rejected_asian_sum_sd, n_rejected_other_sum_sd),
                                       names_pattern = "n_(.+)_sum_sd",
                                       names_to = "kind", values_to = "sd") %>%
  dplyr::select(kind, sd)
rejected <- merge(rejected_mean, rejected_sd, by = "kind")
submitted_mean <- df_sim %>% pivot_longer(cols = c(n_submitted_white_sum_mean, n_submitted_black_sum_mean,
                                                   n_submitted_hispanic_sum_mean, n_submitted_asian_sum_mean, n_submitted_other_sum_mean),
                                          names_pattern = "n_(.+)_sum_mean",
                                          names_to = "kind", values_to = "mean") %>%
  dplyr::select(kind, mean)
submitted_sd <- df_sim %>% pivot_longer(cols = c(n_submitted_white_sum_sd, n_submitted_black_sum_sd,
                                                 n_submitted_hispanic_sum_sd, n_submitted_asian_sum_sd, n_submitted_other_sum_sd),
                                        names_pattern = "n_(.+)_sum_sd",
                                        names_to = "kind", values_to = "sd") %>%
  dplyr::select(kind, sd)
submitted <- merge(submitted_mean, submitted_sd, by = "kind")

requested_mean <- df_sim %>% pivot_longer(cols = c(n_requested_white_sum_mean, n_requested_black_sum_mean,
                                                   n_requested_hispanic_sum_mean, n_requested_asian_sum_mean, n_requested_other_sum_mean),
                                          names_pattern = "n_(.+)_sum_mean",
                                          names_to = "kind", values_to = "mean") %>%
  dplyr::select(kind, mean)
requested_sd <- df_sim %>% pivot_longer(cols = c(n_requested_white_sum_sd, n_requested_black_sum_sd,
                                                 n_requested_hispanic_sum_sd, n_requested_asian_sum_sd, n_requested_other_sum_sd),
                                        names_pattern = "n_(.+)_sum_sd",
                                        names_to = "kind", values_to = "sd") %>%
  dplyr::select(kind, sd)
requested <- merge(requested_mean, requested_sd, by = "kind")
national_sim <- rbind(requested, submitted, rejected) %>% separate(kind, c("kind", "group")) %>%
  as_tibble() %>% mutate(kind = factor(kind, levels = c("requested", "submitted", "rejected")))
plt_national <- ggplot(data = national_sim) + 
  geom_point(aes(x = group, y = mean)) + 
  geom_errorbar(aes(x = group, y = mean, ymax = mean + sd, ymin = mean - sd), width = 0) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(~kind, scale = "free") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  labs(y = "Rejected ballots", title = "N of rejected ballots (70% VBM)")
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_n_rejected_national_turnout_by_state.jpeg"), plt_national)
# change in support
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup()
nsim <- max(df_sim$sim)
dem_white <- rbeta(nsim, as.integer((0.45 * 828)), as.integer(0.48 * 828))
dem_black <- rbeta(nsim, as.integer((0.77 * 149)), as.integer(0.13 * 149))
dem_hisp <- rbeta(nsim, as.integer((0.6 * 143)), as.integer(0.26 * 143))
dem_other <- dem_asian <- rbeta(nsim, as.integer((0.54 * 87)), as.integer(0.29 * 87))
df_sim <- df_sim %>% 
  mutate(biden_voters = rbinom(nrow(.), voters_white_sum - n_rejected_white_sum, prob = dem_white[sim]), + 
                        rbinom(nrow(.), voters_black_sum - n_rejected_black_sum,prob = dem_black[sim]) +
                        rbinom(nrow(.), voters_hispanic_sum - n_rejected_hispanic_sum, prob = dem_hisp[sim]) +
                        rbinom(nrow(.), voters_asian_sum - n_rejected_asian_sum, prob = dem_asian[sim]) +
                        rbinom(nrow(.), voters_other_sum - n_rejected_other_sum, prob = dem_other[sim]),
         support = biden_voters / (voters_white_sum + voters_black_sum + voters_hispanic_sum + voters_asian_sum + voters_other_sum - n_rejected_voters),
         biden_voters_novbm = rbinom(nrow(.), voters_white_sum , prob = dem_white[sim]), + 
           rbinom(nrow(.), voters_black_sum,prob = dem_black[sim]) +
           rbinom(nrow(.), voters_hispanic_sum, prob = dem_hisp[sim]) +
           rbinom(nrow(.), voters_asian_sum, prob = dem_asian[sim]) +
           rbinom(nrow(.), voters_other_sum , prob = dem_other[sim]),
         support_novbm = biden_voters_novbm / (voters_white_sum + voters_black_sum + voters_hispanic_sum + voters_asian_sum + voters_other_sum),
         )
df_sim_support <- df_sim %>% 
  group_by(State) %>%
  summarize(support_mean = mean(support),
            support_sd   = sd(support))
ggplot(data = df_sim_support %>% mutate(State = factor(State, levels = State[order(support_mean)]))) + 
  geom_point(aes(x = State, y = support_mean)) + 
  geom_errorbar(aes(x = State, ymax = support_mean + support_sd, ymin = support_mean - support_sd)) + 
  theme_bw() + 
  theme(axis.title = element_blank(), axis.text.x = element_text(angle = 90)) + 
  labs(title = "Democratic voteshare (70% VBM)", y = "Support")
           
  

## rates ----
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup()
df_shares <- df_sim %>% 
  transmute(
    State = State,
    sim = sim,
    voters_sum = voters_white_sum + voters_black_sum + voters_hispanic_sum + voters_asian_sum + voters_other_sum,
    share_voters_white = voters_white_sum / voters_sum,
    share_voters_black = voters_black_sum / voters_sum,
    share_voters_hispanic = voters_hispanic_sum / voters_sum,
    share_voters_asian = voters_asian_sum / voters_sum,
    share_voters_other = voters_other_sum / voters_sum,
    share_requested_white = n_requested_white_sum / n_requested_sum,
    share_requested_black = n_requested_black_sum / n_requested_sum,
    share_requested_hispanic = n_requested_hispanic_sum / n_requested_sum,
    share_requested_asian = n_requested_asian_sum / n_requested_sum,
    share_requested_other = n_requested_other_sum / n_requested_sum,
    share_submitted_white = n_submitted_white_sum / n_submitted_sum,
    share_submitted_black = n_submitted_black_sum / n_submitted_sum,
    share_submitted_hispanic = n_submitted_hispanic_sum / n_submitted_sum,
    share_submitted_asian = n_submitted_asian_sum / n_submitted_sum,
    share_submitted_other = n_submitted_other_sum / n_submitted_sum,
    share_rejected_white = n_rejected_white_sum / n_rejected_sum,
    share_rejected_black = n_rejected_black_sum / n_rejected_sum,
    share_rejected_hispanic = n_rejected_hispanic_sum / n_rejected_sum,
    share_rejected_asian = n_rejected_asian_sum / n_rejected_sum,
    share_rejected_other = n_rejected_other_sum / n_rejected_sum
  ) %>% 
  dplyr::select(-voters_sum) %>%
  pivot_longer(cols = c(-State, -sim), 
               names_pattern = "share_(.+)_(.+)", 
               names_to = c("kind", "group"), 
               values_to = "share") %>%
  group_by(State, kind, group) %>% 
  dplyr::select(-sim) %>%
  summarize_all(list(mean = mean, sd = sd)) %>%
  ungroup()
rates_white <- ggplot(data = df_shares %>% 
                        filter(group == 'white') %>%
                        mutate(State = factor(State, levels = df_shares %>% 
                                                filter(kind == 'voters', group == "white") %>% 
                                                arrange(mean) %>% 
                                                pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw() + 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "white")
rates_black <- ggplot(data = df_shares %>% 
                        filter(group == 'black') %>%
                        mutate(State = factor(State, levels = df_shares %>% 
                                                filter(kind == 'voters', group == "black") %>% 
                                                arrange(mean) %>% 
                                                pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "black")
rates_hispanic <- ggplot(data = df_shares %>% 
                           filter(group == 'hispanic') %>%
                           mutate(State = factor(State, levels = df_shares %>% 
                                                   filter(kind == 'voters', group == "hispanic") %>% 
                                                   arrange(mean) %>% 
                                                   pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "latinx")
rates_asian <- ggplot(data = df_shares %>% 
                           filter(group == 'asian') %>%
                           mutate(State = factor(State, levels = df_shares %>% 
                                                   filter(kind == 'voters', group == "asian") %>% 
                                                   arrange(mean) %>% 
                                                   pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "asian")
rates_other <- ggplot(data = df_shares %>% 
                        filter(group == 'other') %>%
                        mutate(State = factor(State, levels = df_shares %>% 
                                                filter(kind == 'voters', group == "other") %>% 
                                                arrange(mean) %>% 
                                                pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "right", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "other")
plt_rates <- grid.arrange(rates_white, rates_black, rates_hispanic, rates_asian, rates_other, ncol = 2)
ggsave(paste0("plots/model_state_level_", Sys.Date(), "_group_shares_turnout_by_state.jpeg"), plt_rates, width = 14, height = 8)


