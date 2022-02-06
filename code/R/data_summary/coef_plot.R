cat <- df_subset %>% dplyr::select(group_id, State) %>% distinct()
groups <- c("white", "black", "latinx", 'asian', "other")
kind <- c("requested", "submitted", "rejected")
#beta_region <- rstan::extract(fit, pars = "beta")[[1]]
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
ggsave(paste0("plots/", model_prefix, "_", subset, Sys.Date(), "_pr_with_turnout_by_state.jpeg"), plt_grid)
