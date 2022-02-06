################################################################################
## Compare shares submitted and rejected
################################################################################
## Load data
source("code/R/States/NC/data_cleaning/clean_absentee_nc_2020.R")
################################################################################
# share submitted, share rejected
submitted <- df %>% 
  group_by(ethn) %>%
  summarize(submitted = n()/nrow(df),
            n_submitted = n()) %>%
  ungroup()
rejected <- df %>%
  filter(status == 0) %>%
  group_by(ethn) %>%
  summarize(n_rejected = n()) %>% 
  ungroup() %>%
  mutate(rejected = n_rejected/sum(n_rejected)) %>%
  dplyr::select(ethn, rejected, n_rejected)
joint <- submitted %>% 
  left_join(rejected, by = 'ethn') %>%
  mutate(rejected = ifelse(is.na(rejected), 0, rejected))
# Plot
ggplot(data = joint %>% 
         filter(ethn != "pac"), aes(x = submitted, y = rejected)) + 
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline() +
  geom_point() +
  theme_bw() +
  geom_text(aes(label=ethn),hjust=-0.25, vjust=-0.5, size = 3) +
  labs(x = "Share submitted", y = "Share rejected", title = paste0("Absentee ballots in NC. Date: ", Sys.Date()))
ggsave("plots/States/NC/Shares_rejected_submitted.jpg")
################################################################################