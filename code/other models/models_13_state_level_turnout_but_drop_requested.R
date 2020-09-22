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
         pr2 = mail_ballots_submitted/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other))

region_crosswalk <- df_subset %>% 
  distinct(State, group_id) %>% 
  arrange(group_id)
write_rds(df_subset, path = "model_fits/fit_m13_states_df_subset.Rds")
m13 <- rstan::stan_model("code/stan/v13_ecological_regression_prop.stan")
data_m13 <- list(
  J = nrow(df_subset),
  G = 5,
  S = length(unique(df_subset$group_id)),
  s = df_subset$group_id,
  xbar1 = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "asian_voteshare",  "other_voteshare")],
  ybar = df_subset[,c("pr2", "pr3")]
)
fit_m13_region <- rstan::sampling(m13, data_m13, chains = 4, cores = 4, warmup = 1500, iter = 2250)
write_rds(fit_m13_region, path = "model_fits/fit_m13_states.Rds")


# coefplot
df_subset <- read_rds(path = "model_fits/fit_m13_states_df_subset.Rds")
fit_m13_region <- read_rds(path = "model_fits/fit_m13_states.Rds")
cat <- df_subset %>% dplyr::select(group_id, State) %>% distinct()
groups <- c("white", "black", "latinx", 'asian', "other")
kind <- c("submitted", "rejected")
beta_region <- rstan::extract(fit_m13_region, pars = "beta")[[1]]
# [iter, kind, region, group]
df_region_coef <- matrix(NA, nrow = 0, ncol = 6)
for (i in 1:nrow(cat)){
  for (j in 1:4){
    for (r in 1:2){
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
    kind = factor(kind, levels = c("submitted", "rejected"))
  )
df_mean_rates_region <- df_subset %>%
  group_by(State) %>%
  summarize(
    submitted = mean(pr2, na.rm = TRUE),
    rejected = mean(pr3, na.rm = TRUE)
  ) %>% pivot_longer(cols = c(submitted, rejected), names_to = "kind", values_to = "mean_rate") %>% 
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
coef_plot_region_submitted <- ggplot(data = df_region_coef %>% 
                                       filter(kind == "submitted") %>%
                                       mutate(State = factor(State, levels = df_region_coef %>% 
                                                               filter(kind == 'submitted', group == "average") %>% 
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
                                                              filter(kind == 'submitted', group == "average") %>% 
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
plt_grid <- grid.arrange(coef_plot_region_submitted, coef_plot_region_rejected, ncol = 1)
ggsave(paste0("plots/m13_", Sys.Date(), "_pr_with_turnout_by_state.jpeg"), plt_grid)




