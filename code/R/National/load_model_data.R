################################################################################
## Compile the EAVS / ACS data
################################################################################
# Libraries
library(tidyverse)
library(DirichletReg)
library(boot)
library(gridExtra)
library(rgdal); library(sp); library(GISTools)
################################################################################
# Support vectors
# Regions
west <- c("WA", "OR", "CA", "NV", "UT", "AZ", "CO", "NM", "WY", "ID", "MT")
midwest <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")
south <- c("TX", "OK", "AR", "LA", "MS","AL", "TN","KY", "FL", "GA", "SC", "NC", "VA", "WV", "DC", "MD", "DE")
northeast <- c("PA", "NY", "NJ", "CT", "RI", "MA", "VT", "NH", "ME")
pacific <- c("HI", "AK")
################################################################################
# Functions
turn_numeric <- function(df, pos){
  for (i in 1:ncol(df)){
    if (i != pos) {
      df[, i] <- as.numeric(as.character(df[,i]))
    }
  }
  return(df)
}
################################################################################
# Load data
df <- read.csv("data/eavs_merged_w_acs_2013_2018.csv")
df_turnout <- read.csv("data/turnout_by_state_wabh.csv")
################################################################################
## Fix character formatting
df <- df %>%
  mutate(pr1 = as.numeric(as.character(pr1)),
         pr2 = as.numeric(as.character(pr2)),
         pr3 = as.numeric(as.character(pr3)),
         rejected = as.numeric(as.character(rejected)),
         submitted = as.numeric(as.character(submitted)),
         transmitted = as.numeric(as.character(transmitted)),
         white_count = as.numeric(as.character(white_count)),
         black_count = as.numeric(as.character(black_count)),
         hispanic_count = as.numeric(as.character(hispanic_count)),
         native_percentage = as.numeric(as.character(native_percentage)),
         pac_percentage = as.numeric(as.character(pac_percentage)),
         total_pop = as.numeric(as.character(total_pop)),
         white_percentage = as.numeric(as.character(white_percentage)),
         black_percentage = as.numeric(as.character(black_percentage)),
         hispanic_percentage = as.numeric(as.character(hispanic_percentage))
  )
## Turnout data
df_turnout <- df_turnout %>% 
  mutate(white = round(ifelse(is.na(white), mean(white, na.rm = TRUE), white), 3),
         black = round(ifelse(is.na(black), mean(black, na.rm = TRUE), black), 3),
         hispanic = round(ifelse(is.na(hispanic), mean(hispanic, na.rm = TRUE), hispanic), 3),
         asian = round(ifelse(is.na(asian), mean(asian, na.rm = TRUE), asian), 3),
         mean_to = (white + black + hispanic + asian)/ 4
  )
## States
df <- df %>% 
  left_join(df_turnout, by = "State") %>% mutate(
  region_5 = ifelse(State %in% west,      "west",
             ifelse(State %in% midwest,   "midwest",
             ifelse(State %in% south,     "south",
             ifelse(State %in% northeast, "northeast",
                                          "pacfic"))))
)
## Subset to remove NAs etc
df_subset <- df %>% 
  filter(!State %in% c("CT")) %>%
  filter(!is.na(white_percentage),
         !is.na(black_percentage),
         !is.na(hispanic_percentage),
         !is.na(native_percentage),
         !is.na(asian_percentage),
         !is.na(pac_percentage),
         !is.na(voting_age),
         !is.na(transmitted),
         !is.na(pr1),
         !is.na(pr2),
         !is.na(pr3),
         !is.na(rejected),
         !is.na(submitted)) %>% 
  mutate(other_percentage = native_percentage + pac_percentage,
         voters_white = white * white_count,
         voters_black = black * black_count,
         voters_hispanic = hispanic * hispanic_count,
         voters_asian    = asian * asian_count,
         other_count = total_pop - white_count - black_count - hispanic_count - asian_count,
         voters_other = mean_to * other_count,
         white_voteshare = 
           voters_white/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other),
         black_voteshare = 
           voters_black/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other),
         hispanic_voteshare = 
           voters_hispanic/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other),
         asian_voteshare = 
           voters_asian/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other),
         other_voteshare = 
           voters_other/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other),
         voters_white = floor(white_voteshare * population),
         voters_black = floor(black_voteshare * population),
         voters_hispanic = floor(hispanic_voteshare * population),
         voters_asian = floor(asian_voteshare * population),
         voters_other = floor(other_voteshare * population),
         voters_count = population,
         pr1 = transmitted/(voters_white + voters_black + voters_hispanic + voters_asian + voters_other),
         pr1 = ifelse(State %in% c("OR", "WA", "CO", "UT", "HI"), 1, pr1),
         pr2 = ifelse(State %in% c("OR"), 1, pr2)
        ) %>%
  filter(pr1 >= 0, pr1 <= 1,
         pr2 >= 0, pr2 <= 1,) 
################################################################################
## Print results
cat("pr1 out of bounds", sum(as.integer(df_subset$pr1 > 1 | df_subset$pr1 < 0)), "\n",
    "pr2 out of bounds", sum(as.integer(df_subset$pr2 > 1 | df_subset$pr2 < 0)), "\n",
    "pr3 out of bounds", sum(as.integer(df_subset$pr3 > 1 | df_subset$pr3 < 0))
)
print(df_subset[sample(1:10), 
                c("JurisdictionName","voting_age","white_voteshare", "black_voteshare", 
                  "hispanic_voteshare", "asian_voteshare", "other_voteshare")])
################################################################################