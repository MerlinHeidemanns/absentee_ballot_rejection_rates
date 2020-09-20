
source("code/load_model_data.R")
library(tidyverse); library(gridExtra); library(rgdal); library(sp); library(GISTools)
library(sf)
# asian
(ggplot(df_subset %>% filter(total_votes_2016 > 1000), aes(x=white_voteshare,
                                                           y= mail_ballots_submitted/total_votes_2016)) +
    geom_point(alpha = 0.4) +
    stat_summary_bin(fun.y='mean', bins=20,
                     color='orange', size=2, geom='point')) + 
  facet_wrap(~State)

# rates vs groups
groups <- df_subset %>% dplyr::select(FIPSCode, white_voteshare, black_voteshare, hispanic_voteshare, 
                     asian_voteshare, other_voteshare) %>%
  pivot_longer(cols = c(white_voteshare, black_voteshare, hispanic_voteshare, 
                        asian_voteshare, other_voteshare),
               names_to = "group", values_to = "share", names_pattern = "(.+)_.+")
prs <- df_subset %>% dplyr::select(FIPSCode, pr1, pr2, pr3) %>%
  pivot_longer(cols = c(pr1, pr2, pr3),
               names_to = "kind", values_to = "prob")
groups_prs <- merge(groups, prs, by = "FIPSCode") %>%
  mutate(kind = ifelse(kind == "pr1", "requested",
                ifelse(kind == "pr2", "submitted",
                       "rejected")),
         group = ifelse(group == "hispanic", "latinx", group))
ggplot(data = groups_prs, aes(x = share, y = prob)) + 
  geom_point(size = 0.3, alpha = 0.5) + 
  facet_grid(kind ~ group) + 
  theme_bw() + 
  labs(y = "Probability to ...", x = "Share of population")
ggsave("plots/scatter_group_v_prob.png")

# spatial
counties <- rgdal::readOGR(dsn = 
                          "data/shapefiles", 
                        layer = "cb_2019_us_county_20m")
counties@data <- counties@data %>% 
  mutate(FIPSCode = as.integer(paste0(STATEFP, COUNTYFP))) %>%
  left_join(df, by = "FIPSCode")
# plotting
counties@data$id <- rownames(counties@data)
counties_points <- fortify(counties, region = "id")
counties_df <- merge(counties_points, counties@data, by = "id")
  
ggplot(counties_df %>% filter(State != "AR", State != "HI", pr3 < 0.05), aes(long, lat, fill = pr3)) +
  geom_polygon(aes(group = group)) +
  facet_wrap(~State, scales = "free")


counties_sf <- st_as_sf(counties)
counties_tbl <- as_tibble(counties_sf)
ggplot(data = counties_sf %>% filter(region_5 == "west"), aes(geometry = geometry)) +
  geom_sf(aes(fill = pr1)) 