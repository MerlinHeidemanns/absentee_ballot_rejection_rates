source("code/load_model_data.R")
turn_numeric <- function(df, pos){
  for (i in 1:ncol(df)){
    if (i != pos) {
      df[, i] <- as.numeric(as.character(df[,i]))
    }
  }
  return(df)
}
req_sub <- ggplot(data = df_subset, aes(x = pr1, y = pr2)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = df_subset, aes(x = pr2, y = pr3)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = df_subset, aes(x = pr1, y = pr3)) + 
  geom_point(size = 0.2)
grid.arrange(req_sub, sub_rej, req_rej)
# model
df_subset  <- df_subset %>% 
  group_by(State) %>%
  filter(n() > 2) %>%
  ungroup() %>%
  mutate(group_id = group_indices(., State),
         pr2 = mail_ballots_submitted/(rejected + voters_white + voters_black + voters_hispanic + voters_asian + voters_other))

region_crosswalk <- df_subset %>% 
  distinct(State, group_id) %>% 
  arrange(group_id)
write_rds(df_subset, path = "model_fits/fit_m12_states_df_subset.Rds")
m11 <- rstan::stan_model("code/stan/v12_ecological_regression_prop.stan")
data_m11 <- list(
  J = nrow(df_subset),
  G = 5,
  S = length(unique(df_subset$group_id)),
  s = df_subset$group_id,
  xbar1 = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")],
  ybar = df_subset[,c("pr2", "pr3")]
)
fit_m12_region <- rstan::sampling(m12, data_m11, chains = 4, cores = 4, warmup = 1500, iter = 2250)
write_rds(fit_m11_region, path = "model_fits/fit_m12_states.Rds")