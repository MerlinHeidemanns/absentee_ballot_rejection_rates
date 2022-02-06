################################################################################
## Look at the raw rejection rate data from EAVS against ethnic composition at
## the county level
################################################################################
## Load libraries
library(tidyverse)
library(gridExtra)
library(rgdal)
library(sp)
library(GISTools)
################################################################################
## Load data through function call
source("code/R/National/load_model_data.R")
################################################################################
## Rates for requested, submitted, rejected against group shares
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

# Distribution of the rejection rate geographically
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
