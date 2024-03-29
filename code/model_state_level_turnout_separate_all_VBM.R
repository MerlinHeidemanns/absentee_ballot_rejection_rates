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

model_prefix <- "model_state_level_allVBM"
subset       <- "none"
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
cat("Size before limits", dim(df_subset), "\n",
    "N of states", length(unique(df_subset$State)))
df_subset  <- df_subset %>% 
  group_by(State) %>%
  filter(n() > 2) %>%
  ungroup() 
cat("Size after limits", dim(df_subset), "\n",
    "N of states", length(unique(df_subset$State)))

df_subset_xbar1 <- df_subset %>%
  filter(!(State %in% c("OR", "WA", "CO", "UT", "HI"))) %>%
  mutate(group_id = group_indices(., State))
df_subset_xbar2 <- df_subset %>% 
  filter(State %in% c("WA", "CO", "UT", "HI")) %>%
    mutate(group_id = max(df_subset_xbar1$group_id) + group_indices(., State))
df_subset_xbar3 <- df_subset %>% 
  filter(State %in% c("OR")) %>%
  mutate(group_id = max(df_subset_xbar2$group_id) + group_indices(., State))
df_subset <- rbind(df_subset_xbar1,
                   df_subset_xbar2,
                   df_subset_xbar3)

region_crosswalk <- rbind(
  df_subset_xbar1 %>% distinct(State, group_id),
  df_subset_xbar2 %>% distinct(State, group_id),
  df_subset_xbar3 %>% distinct(State, group_id)
) %>% arrange(group_id)

write_rds(df_subset, path = paste0("model_fits/", model_prefix, "_", subset, "_data", ".Rds"))

# model
model <- rstan::stan_model("code/stan/v11_ecological_regression_prop_w_allVBM.stan")

J <- c(nrow(df_subset_xbar1), 
       nrow(df_subset_xbar2), 
       nrow(df_subset_xbar3))
G <- 5
S <- c(length(unique(df_subset_xbar1$group_id)),
       length(unique(df_subset_xbar1$group_id)) + length(unique(df_subset_xbar2$group_id)),
       length(unique(df_subset_xbar1$group_id)) + length(unique(df_subset_xbar2$group_id)) + length(unique(df_subset_xbar3$group_id))
       )
s1 <- df_subset_xbar1$group_id
s2 <- c(df_subset_xbar1$group_id, df_subset_xbar2$group_id)
s3 <- c(df_subset_xbar1$group_id, df_subset_xbar2$group_id, df_subset_xbar3$group_id)
xbar1 <- df_subset_xbar1[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")]
xbar2_add <- df_subset_xbar2[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")]
xbar3_add <- df_subset_xbar3[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")]
ybar1 <- df_subset_xbar1[, c("pr1")] %>% pull(pr1)
ybar2 <- rbind(df_subset_xbar1[, c("pr2")], df_subset_xbar2[, c("pr2")]) %>% pull(pr2)
ybar3 <- rbind(df_subset_xbar1[, c("pr3")], 
               df_subset_xbar2[, c("pr3")], 
               df_subset_xbar3[, c("pr3")]) %>% pull(pr3)
data_model <- list(
  J = J,
  G = G,
  S = S,
  s1 = s1,
  s2 = s2,
  s3 = s3,
  xbar1 = xbar1,
  xbar2_add = xbar2_add,
  xbar3_add = xbar3_add,
  ybar1 = ybar1,
  ybar2 = ybar2,
  ybar3 = ybar3
)
fit <- rstan::sampling(model, data_model, chains = 4, cores = 4, warmup = 2500, iter = 3500)
write_rds(fit, path = paste0("model_fits/", model_prefix, "_", subset, ".Rds"))


# prediction
# create posterior compositions of the groups of those who requested and submitted absentee ballots
fit <- read_rds(path = paste0("model_fits/", model_prefix, "_", subset, ".Rds"))
# 
# df_subset <- read_rds(path = paste0("model_fits/", model_prefix, "_", subset, "_data", ".Rds"))
# 
# nsim = 100
# 
# beta_1 <- rstan::extract(fit, pars = "beta1")[[1]]
# beta_2 <- rstan::extract(fit, pars = "beta2")[[1]]
# beta_3 <- rstan::extract(fit, pars = "beta3")[[1]]
# beta_region <- array(1, dim = c(2000, 3, 46, 5))
# beta_region[,1,1:42,] <- beta_1
# beta_region[,2,1:45,] <- beta_2
# beta_region[,3,1:46,] <- beta_3
# 
# sigma <- rstan::extract(fit, pars = "sigma")[[1]]
# counter = 0
# # for (i in sample(1:3000, nsim,replace = FALSE)){
# #   counter = counter + 1
# #   xbar2 <- beta_region[i, 1, data_model$s,] * data_model$xbar1
# #   xbar2 <- xbar2/rowSums(xbar2)
# #   xbar3 <- beta_region[i, 2, data_model$s,] * xbar2
# #   xbar3 <- xbar3/rowSums(xbar3)
# #   ybar[,1, counter] <- rnorm_truncated(rowSums(beta_region[i, 1, data_model$s,] * data_model$xbar1), sigma[i, 1], 0, 1)
# #   ybar[,2, counter] <- rnorm_truncated(rowSums(beta_region[i, 2, data_model$s,] * xbar2), sigma[i, 2], 0, 1)
# #   ybar[,3, counter] <- rnorm_truncated(rowSums(beta_region[i, 3, data_model$s,] * xbar3), sigma[i, 3], 0, 1)
# # }
# for (i in sample(1:3000, nsim,replace = FALSE)){
#   counter = counter + 1
#   xbar2 <- beta_region[i, 1, data_model$s,] * data_model$xbar1
#   xbar2 <- xbar2/rowSums(xbar2)
#   xbar3 <- beta_region[i, 2, data_model$s,] * xbar2
#   xbar3 <- xbar3/rowSums(xbar3)
#   ybar[,1, counter] <- rnorm(data_model$J, rowSums(beta_region[i, 1, data_model$s,] * data_model$xbar1), sigma[i, 1])
#   ybar[,2, counter] <- rnorm(data_model$J, rowSums(beta_region[i, 2, data_model$s,] * xbar2), sigma[i, 2])
#   ybar[,3, counter] <- rnorm(data_model$J, rowSums(beta_region[i, 3, data_model$s,] * xbar3), sigma[i, 3])
# }
# ybar[ybar > 1] <- 1
# ybar[ybar < 0] <- 0
# # overlap
# # national
# coverage_025_975_requested <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 1] > quantile(ybar[x, 1,], 0.025)) & (data_model$ybar[x, 1] < quantile(ybar[x, 1,], 0.975)) ))
# coverage_025_975_submitted <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 2] > quantile(ybar[x, 2,], 0.025)) & (data_model$ybar[x, 2] < quantile(ybar[x, 2,], 0.975)) ))
# coverage_025_975_rejected <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 3] > quantile(ybar[x, 3,], 0.025)) & (data_model$ybar[x, 3] < quantile(ybar[x, 3,], 0.975)) ))
# cat("95% interval coverage for requested:", round(mean(coverage_025_975_requested), 4), "\n",
#     "95% interval coverage for submitted:", round(mean(coverage_025_975_submitted), 4), "\n",
#     "95% interval coverage for rejected:", round(mean(coverage_025_975_rejected), 4))
# coverage_25_75_requested <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 1] > quantile(ybar[x, 1,], 0.25)) & (data_model$ybar[x, 1] < quantile(ybar[x, 1,], 0.75)) ))
# coverage_25_75_submitted <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 2] > quantile(ybar[x, 2,], 0.25)) & (data_model$ybar[x, 2] < quantile(ybar[x, 2,], 0.75)) ))
# coverage_25_75_rejected <- sapply(1:data_model$J, function(x) as.integer((data_model$ybar[x, 3] > quantile(ybar[x, 3,], 0.25)) & (data_model$ybar[x, 3] < quantile(ybar[x, 3,], 0.75)) ))
# cat("50% interval coverage for requested:", round(mean(coverage_25_75_requested), 4), "\n",
#     "50% interval coverage for submitted:", round(mean(coverage_25_75_submitted), 4), "\n",
#     "50% interval coverage for rejected:", round(mean(coverage_25_75_rejected), 4))
# # state-level
# # compute interval coverage on the state level
# coverage <- data.frame(State = region_crosswalk[as.integer(data_model$s),"State"] %>% pull(State),
#                        requested = coverage_025_975_requested,
#                        submitted = coverage_025_975_submitted,
#                        rejected = coverage_025_975_rejected)
# coverage <- coverage %>% group_by(State) %>%
#   summarize_all(list(mean = mean)) %>%
#   pivot_longer(cols = c(requested_mean, submitted_mean, rejected_mean), 
#                names_to = "kind", names_pattern = "(.+)_.+", values_to = "mean")
# ggplot(coverage %>% mutate(State = factor(State, levels = coverage %>%
#                                             filter(kind == "requested") %>%
#                                             arrange(mean) %>%
#                                             pull(State))), aes(x = State, y = mean, color = kind)) + 
#   geom_point(position = position_dodge(width = 0.5)) +
#   geom_hline(yintercept = 0.95) + 
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + 
#   labs(y = "95% interval coverage")
# coverage <- data.frame(State = region_crosswalk[as.integer(data_model$s),"State"] %>% pull(State),
#                        requested = coverage_25_75_requested,
#                        submitted = coverage_25_75_submitted,
#                        rejected = coverage_25_75_rejected)
# coverage <- coverage %>% group_by(State) %>%
#   summarize_all(list(mean = mean)) %>%
#   pivot_longer(cols = c(requested_mean, submitted_mean, rejected_mean), 
#                names_to = "kind", names_pattern = "(.+)_.+", values_to = "mean")
# ggplot(coverage %>% mutate(State = factor(State, levels = coverage %>%
#                                             filter(kind == "requested") %>%
#                                             arrange(mean) %>%
#                                             pull(State))), aes(x = State, y = mean, color = kind)) + 
#   geom_point(position = position_dodge(width = 0.5)) + 
#   geom_hline(yintercept = 0.5) + 
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank()) + 
#   labs(y = "50% interval coverage")
# 
# # predicted values
# ## requested
# ybar_requested <- as.data.frame(cbind(data_model$s, ybar[,1,]))
# names(ybar_requested)[1] <- "group_id"
# ybar_requested <- ybar_requested %>% pivot_longer(cols = c(-group_id), values_to = "y", names_to = "sim", names_prefix = "V")
# ybar_requested$State <- region_crosswalk[ybar_requested$group_id,] %>% pull(State)
# ybar_requested <- ybar_requested %>% dplyr::select(y, State) %>% mutate(kind = "predicted")
# ybar_requested_true <- data.frame(y = df_subset$pr1, State = df_subset$State, kind = "true")
# ybar_requested <- bind_rows(ybar_requested, ybar_requested_true)
# ppd_requested <- ggplot(data = ybar_requested, aes(x = y, fill = kind)) + 
#   geom_histogram(stat = "density", position = "identity", alpha = 0.5) + 
#   facet_wrap(State ~ ., scales = "free_y") + 
#   labs(title = "Requested PPD") + 
#   theme_bw() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_requested_by_state.jpeg"), ppd_requested)
# ## submitted
# ybar_submitted <- as.data.frame(cbind(data_model$s, ybar[,2,]))
# names(ybar_submitted)[1] <- "group_id"
# ybar_submitted <- ybar_submitted %>% 
#   pivot_longer(cols = c(-group_id), values_to = "y", names_to = "sim", names_prefix = "V")
# ybar_submitted$State <- region_crosswalk[ybar_submitted$group_id,] %>% pull(State)
# ybar_submitted <- ybar_submitted %>% dplyr::select(y, State) %>% mutate(kind = "predicted")
# ybar_submitted_true <- data.frame(y = df_subset$pr2, State = df_subset$State, kind = "true")
# ybar_submitted <- bind_rows(ybar_submitted, ybar_submitted_true)
# ppd_submitted <- ggplot(data = ybar_submitted, aes(x = y, fill = kind)) + 
#   geom_histogram(stat = "density", position = "identity", alpha = 0.5) + 
#   facet_wrap(State ~ ., scales = "free_y") + 
#   labs(title = "Submitted PPD") + 
#   theme_bw() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave(paste0("plots/m11_", Sys.Date(), "_ppd_submitted_by_state.jpeg"), ppd_submitted)
# ## rejected
# ybar_rejected <- as.data.frame(cbind(data_model$s, ybar[,3,]))
# names(ybar_rejected)[1] <- "group_id"
# ybar_rejected <- ybar_rejected %>% 
#   pivot_longer(cols = c(-group_id), values_to = "y", names_to = "sim", names_prefix = "V")
# ybar_rejected$State <- region_crosswalk[ybar_rejected$group_id,] %>% pull(State)
# ybar_rejected <- ybar_rejected %>% dplyr::select(y, State) %>% mutate(kind = "predicted")
# ybar_rejected_true <- data.frame(y = df_subset$pr3, State = df_subset$State, kind = "true")
# ybar_rejected <- bind_rows(ybar_rejected, ybar_rejected_true)
# ppd_rejected <- ggplot(data = ybar_rejected, aes(x = y, fill = kind)) + 
#   geom_histogram(stat = "density", position = "identity", alpha = 0.5) + 
#   facet_wrap(State ~ ., scales = "free_y") + 
#   labs(title = "Rejected PPD") + 
#   theme_bw() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_rejected_by_state.jpeg"), ppd_rejected)
# ppd_requested
# ppd_submitted
# ppd_rejected
# 
# # minimum
# minimum_predicted <- as.data.frame(t(apply(ybar, MARGIN = c(2,3), min)))
# colnames(minimum_predicted) <- c("requested", "submitted", "rejected")
# minimum_predicted %>% pivot_longer(cols = everything(), names_to = "kind", values_to = "ybar") %>%
#   ggplot(.) + 
#   geom_histogram(aes(x = ybar)) + 
#   facet_wrap(kind ~ ., scales = "free") +
#   theme_bw() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
#   labs(title = "Predicted minimum")
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_minimum.jpeg"), minimum_predicted)
# maximum_predicted <- as.data.frame(t(apply(ybar, MARGIN = c(2,3), max)))
# colnames(maximum_predicted) <- c("requested", "submitted", "rejected")
# maximum_predicted %>% pivot_longer(cols = everything(), names_to = "kind", values_to = "ybar") %>%
#   ggplot(.) + 
#   geom_histogram(aes(x = ybar)) + 
#   facet_wrap(kind ~ ., scales = "free") +
#   theme_bw() + 
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank()) + 
#   labs(title = "Predicted maximum")
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_ppd_maximum.jpeg"), maximum_predicted)
# 
# 
# 
# # residuals ---------------------------------------------------------------
# counties <- rgdal::readOGR(dsn = 
#                             "data/shapefiles", 
#                           layer = "cb_2019_us_county_20m")
# ybar_mean <- apply(ybar, MARGIN = c(1, 2), mean)
# df_residuals <- data.frame(FIPSCode = df_subset$FIPSCode, 
#                            y1 = df_subset$pr1,
#                            y2 = df_subset$pr2,
#                            y3 = df_subset$pr3,
#                            y1_hat = ybar_mean[, 1],
#                            y2_hat = ybar_mean[, 2],
#                            y3_hat = ybar_mean[, 3]
# ) %>% 
#   mutate(
#     residuals1 = y1 - y1_hat,
#     residuals2 = y2 - y2_hat,
#     residuals3 = y3 - y3_hat
#   )
# counties@data <- counties@data %>% 
#   mutate(FIPSCode = as.integer(paste0(STATEFP, COUNTYFP))) %>%
#   left_join(df_residuals, by = "FIPSCode") %>%
#   left_join(df_subset, by = "FIPSCode")
# # plotting
# counties@data$id <- rownames(counties@data)
# counties_points <- fortify(counties, region = "id")
# counties_df <- merge(counties_points, counties@data, by = "id")
# res1 <- ggplot(counties_df %>% filter(State != "HI", State != "AK"), aes(long, lat, fill = residuals1)) +
#   geom_polygon(aes(group = group)) +
#   facet_wrap(~State, scales = "free")
# res2 <- ggplot(counties_df %>% filter(State != "HI", State != "AK"), aes(long, lat, fill = residuals2)) +
#   geom_polygon(aes(group = group)) +
#   facet_wrap(~State, scales = "free")
# res3 <- ggplot(counties_df %>% filter(State != "HI", State != "AK"), aes(long, lat, fill = residuals3)) +
#   geom_polygon(aes(group = group)) +
#   facet_wrap(~State, scales = "free")
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_residuals_requested.jpeg"), res1)
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_residuals_submitted.jpeg"), res2)
# ggsave(paste0("plots/model_state_level_", Sys.Date(), "_residuals_rejected.jpeg"), res3)
# 

# coefplot ----------------------------------------------------------------

beta_1 <- rstan::extract(fit, pars = "beta1")[[1]]
beta_2 <- rstan::extract(fit, pars = "beta2")[[1]]
beta_3 <- rstan::extract(fit, pars = "beta3")[[1]]
beta_region <- array(1, dim = c(4000, 3, 48, 5))
beta_region[,1,1:43,] <- beta_1
beta_region[,2,1:47,] <- beta_2
beta_region[,3,1:48,] <- beta_3
source("code/data_summary/coef_plot.R")

# voting power 
beta_1 <- rstan::extract(fit, pars = "beta1")[[1]]
beta_2 <- rstan::extract(fit, pars = "beta2")[[1]]
beta_3 <- rstan::extract(fit, pars = "beta3")[[1]]
beta_region <- array(1, dim = c(4000, 3, 48, 5))
beta_region[,1,1:43,] <- beta_1
beta_region[,2,1:47,] <- beta_2
beta_region[,3,1:48,] <- beta_3

source("code/data_summary/composition_simulation.R")
absentee_data <- read.csv("data/GE2020/absentee_ballot_requests.csv")
df_sim <- simulation_function(100, 4000, 0.7, absentee_data = absentee_data, beta_draws = beta_region)
write_rds(df_sim, path = "model_fits/fit_simulated_70percent_turnout_by_state_allVBM_requested.Rds")

# simulation plots
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state_allVBM_requested.Rds")
df_sim <- test %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup() %>% 
  group_by(State) %>%
  summarize_all(list(~ mean(.), ~ sd(.))) # %>% 
  #write.csv(., file = paste0(model_prefix, "_", subset, "_estimates_by_state_group_updated.csv"))
## black ----
plt_n_black_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_black_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_black_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_black_sum_mean + n_rejected_black_sum_sd,
                    ymin = n_rejected_black_sum_mean - n_rejected_black_sum_sd), width = 0) +
  scale_y_continuous(labels = comma) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Black absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(), "_n_rejected_black_by_state_turnout_by_state.jpeg"), plt_n_black_rejected)
## hispanic ----
plt_n_latinx_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_hispanic_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_hispanic_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_hispanic_sum_mean + n_rejected_hispanic_sum_sd,
                    ymin = n_rejected_hispanic_sum_mean - n_rejected_hispanic_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Latinx absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(), "_n_rejected_latinx_by_state_turnout_by_state.jpeg"), plt_n_latinx_rejected)
## asian ----
plt_n_asian_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_asian_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_asian_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_asian_sum_mean + n_rejected_asian_sum_sd,
                    ymin = n_rejected_asian_sum_mean - n_rejected_asian_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Asian absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(),"_n_asian_by_state_turnout_by_state.jpeg"), plt_n_asian_rejected)
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
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(), "_n_rejected_white_by_state_turnout_by_state.jpeg"), plt_n_white_rejected)
## nation ----
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state_allVBM_requested.Rds")
df_sim <- df_sim %>% group_by(sim) %>% dplyr::select(-State) %>%
  summarise_all(list(sum = sum)) %>% 
  ungroup() %>% 
  summarize_all(list(~ mean(.), ~ sd(.)))
  
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
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(), "_n_rejected_national_turnout_by_state.jpeg"), plt_national)
# change in support
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state_allVBM_requested.Rds")
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
df_sim <- read_rds(path = "model_fits/fit_simulated_70percent_turnout_by_state_allVBM_requested.Rds")
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
  summarize_all(list(~ mean(.), ~ sd(.))) %>%
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
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(),"_group_shares_turnout_by_state.jpeg"), plt_rates, width = 14, height = 8)


# bubble graph
source("code/data_summary/bubble_plot.R")

