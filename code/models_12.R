# model 12
source("code/load_model_data.R")
# data
m11 <- rstan::stan_model("code/stan/v11_ecological_regression_prop.stan")
turnout <- c(0.647, 0.599, 0.449, 0.463)
names(turnout) <- c("white", "black", "hispanic",'other')

df_subset <- df %>% 
  filter(!State %in% c("CT", "HI")) %>%
  filter(!is.na(white_percentage),
         !is.na(black_percentage),
         !is.na(hispanic_percentage),
         !is.na(native_percentage),
         !is.na(asian_percentage),
         !is.na(pac_percentage),
         !is.na(other_percentage),
         !is.na(voting_age),
         !is.na(transmitted),
         !is.na(pr1),
         !is.na(pr2),
         !is.na(pr3),
         !is.na(rejected),
         !is.na(mail_ballots_submitted)) %>%
  mutate(other_percentage = other_percentage + native_percentage + pac_percentage,
         other_percentage = (1 - white_percentage - black_percentage - hispanic_percentage),
         other_percentage = ifelse(other_percentage < 0, 0, other_percentage),
         voters_white     = floor(turnout[1] * white_percentage * voting_age_2),
         voters_black     = floor(turnout[2] * black_percentage * voting_age_2),
         voters_hispanic  = floor(turnout[3] * hispanic_percentage * voting_age_2),
         voters_other     = floor(turnout[4] * other_percentage * voting_age_2),
         white_voteshare = voters_white/(voters_white + voters_black + voters_hispanic + voters_other),
         black_voteshare = voters_black/(voters_white + voters_black + voters_hispanic + voters_other),
         hispanic_voteshare = voters_hispanic/(voters_white + voters_black + voters_hispanic + voters_other),
         other_voteshare = voters_other/(voters_white + voters_black + voters_hispanic + voters_other),
         voters_white = floor(white_voteshare * total_votes_2016),
         voters_black = floor(black_voteshare * total_votes_2016),
         voters_hispanic = floor(hispanic_voteshare * total_votes_2016),
         voters_other = floor(other_voteshare * total_votes_2016),
         voters_count = voters_white + voters_black + voters_hispanic + voters_other,
         pr1 = transmitted/(voters_white + voters_black + voters_hispanic + voters_other),
         pr2 = mail_ballots_submitted/transmitted,
         pr3 = rejected/mail_ballots_submitted) %>%
  filter((pr1 >= 0) & (pr1 <= 1)) %>%
  mutate(group_id = group_indices(., region))
print(df_subset[sample(1:nrow(df_subset)), c("JurisdictionName","voting_age_2","voters_white", "voters_black", "voters_hispanic", "voters_other")])
cat("Implied total voters:  ", sum(df_subset[,c("voters_white", "voters_black", "voters_hispanic", "voters_other")]), "\n")
print("Min size:")
df_subset %>% 
  select(State, total_votes_2016, white_voteshare, black_voteshare, hispanic_voteshare, other_voteshare) %>% 
  arrange(total_votes_2016) %>% 
  slice(1:10)
cat("pr1 out of bounds", sum(as.integer(df_subset$pr1 > 1 | df_subset$pr1 < 0)), "\n",
    "pr2 out of bounds", sum(as.integer(df_subset$pr2 > 1 | df_subset$pr2 < 0)), "\n",
    "pr3 out of bounds", sum(as.integer(df_subset$pr3 > 1   | df_subset$pr3 < 0))
)

