source("code/States/NC/clean_absentee_nc.R")
# share submitted, share rejected
submitted <- df %>% 
  group_by(ethn) %>%
  summarize(submitted = n()/nrow(df),
            n = n()) %>%
  ungroup()
rejected <- df %>%
  filter(status == 0) %>%
  group_by(ethn) %>%
  summarize(n = n()) %>% 
  ungroup() %>%
  mutate(rejected = n/sum(n)) %>%
  dplyr::select(ethn, rejected)
joint <- submitted %>% 
  left_join(rejected, by = 'ethn') %>%
  mutate(rejected = ifelse(is.na(rejected), 0, rejected))
ggplot(data = joint %>% 
         filter(ethn != "pac"), aes(x = submitted, y = rejected)) + 
  lims(x = c(0, 1), y = c(0, 1)) +
  geom_abline() +
  geom_point() +
  theme_bw() +
  geom_text(aes(label=ethn),hjust=-0.25, vjust=-0.5, size = 3) +
  labs(x = "Share submitted", y = "Share rejected", title = paste0("Absentee ballots in NC. Date: ", Sys.Date()))
ggsave("plots/States/NC/Shares_rejected_submitted.jpg")

