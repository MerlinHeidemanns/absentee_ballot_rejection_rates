source("code/load_model_data.R")
req_sub <- ggplot(data = df_subset, aes(x = pr1, y = pr2)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = df_subset, aes(x = pr2, y = pr3)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = df_subset, aes(x = pr1, y = pr3)) + 
  geom_point(size = 0.2)
grid.arrange(req_sub, sub_rej, req_rej)
# model
m8 <- rstan::stan_model("code/stan/v8_ecological_regression_prop.stan")
data_m8 <- list(
  J = nrow(df_subset),
  G = 5,
  xbar1 = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")],
  ybar = df_subset[,c("pr1", "pr2", "pr3")]
)
fit_m8 <- rstan::sampling(m8, data_m8, chains = 2)

## coef
categories <- c("white", "black", "latinx", "asian", "other")
kind <- c("requesting", "submitting", "rejection")
beta <- rstan::extract(fit_m8, pars = "beta")[[1]]
beta_mean <- apply(beta, MARGIN = c(2, 3), mean)
beta_sd   <- apply(beta, MARGIN = c(2, 3), sd)
plt_df <- matrix(NA, ncol = 4, nrow = 0)
for (i in 1:3){
  plt_df <- rbind(plt_df, cbind(categories, beta_mean[,i], beta_sd[,i], kind[i]))
}
plt_df <- as.data.frame(plt_df)
colnames(plt_df) <- c("ethn", "mean_rate", "sd_rate", "kind")
plt_df <- plt_df %>% 
  mutate(mean_rate = round(as.numeric(as.character(mean_rate)), 5),
         sd_rate   = round(as.numeric(as.character(sd_rate)), 5))
ggplot(data = plt_df) + 
  geom_point(aes(x = ethn, y = mean_rate)) + 
  geom_errorbar(aes(x = ethn, y = mean_rate, ymax = mean_rate + sd_rate, ymin = mean_rate - sd_rate)) +
  facet_wrap(~kind, scales = "free")

