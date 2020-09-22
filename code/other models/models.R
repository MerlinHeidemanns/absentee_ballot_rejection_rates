# v4

# libraries
library(tidyverse)
library(DirichletReg)
library(boot)
library(gridExtra)
# regions
west <- c("WA", "OR", "CA", "NV", "UT", "AZ", "CO", "NM", "WY", "ID", "MT")
midwest <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "IN", "MI", "OH")
south <- c("TX", "OK", "AR", "LA", "MS","AL", "TN","KY", "FL", "GA", "SC", "NC", "VA", "WV", "DC", "MD", "DE")
northeast <- c("PA", "NY", "NJ", "CT", "RI", "MA", "VT", "NH", "ME")
pacific <- c("HI", "AK")
# functions
turn_numeric <- function(df, pos){
  for (i in 1:ncol(df)){
    if (i != pos) {
      df[, i] <- as.numeric(as.character(df[,i]))
    }
  }
  return(df)
}
# data
df <- read.csv("data/eavs_merged_w_acs15_18_2020_08_20.csv") %>%
  mutate(pr1 = as.numeric(as.character(pr1)),
         pr1 = ifelse(pr1 >1 | pr1 < 0, NA, pr1),
         pr2 = as.numeric(as.character(pr2)),
         pr2 = ifelse(pr1 >1 | pr1 < 0, NA, pr2),
         pr3 = as.numeric(as.character(pr3)),
         pr3 = ifelse(pr1 >1 | pr1 < 0, NA, pr3),
         rejected = as.numeric(as.character(rejected)),
         mail_ballots_submitted = as.numeric(as.character(mail_ballots_submitted)),
         transmitted = as.numeric(as.character(transmitted)),
         white_count = as.numeric(as.character(white_count)),
         black_count = as.numeric(as.character(black_count)),
         hispanic_count = as.numeric(as.character(hispanic_count)),
         other_percentage = as.numeric(as.character(other_percentage)),
         native_percentage = as.numeric(as.character(native_percentage)),
         pac_percentage = as.numeric(as.character(pac_percentage)),
         total_pop = as.numeric(as.character(total_pop)),
         white_percentage = as.numeric(as.character(white_percentage)),
         black_percentage = as.numeric(as.character(black_percentage)),
         hispanic_percentage = as.numeric(as.character(hispanic_percentage))
  )
df_turnout <- read.csv("data/turnout_by_state_wabh.csv") %>% 
  mutate(white = round(ifelse(is.na(white), mean(white, na.rm = TRUE), white), 3),
         black = round(ifelse(is.na(black), mean(black, na.rm = TRUE), black), 3),
         hispanic = round(ifelse(is.na(hispanic), mean(hispanic, na.rm = TRUE), hispanic), 3),
         asian = round(ifelse(is.na(asian), mean(asian, na.rm = TRUE), asian), 3),
         mean_to = (white + black + hispanic + asian)/ 4
         )
df <- df %>% left_join(df_turnout, by = "State") %>% mutate(
  region_5 = ifelse(State %in% west,      "west",
             ifelse(State %in% midwest,   "midwest",
             ifelse(State %in% south,     "south",
             ifelse(State %in% northeast, "northeast",
                   "pacfic"))))
)
  
# model 4 ----
model <- rstan::stan_model("code/stan/v4_ecological_regression_prop.stan")
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
         pr1 = transmitted/voting_age,
         pr2 = mail_ballots_submitted/transmitted,
         pr3 = rejected/mail_ballots_submitted)
cat(sum(as.integer(df_subset$pr3 > 1)))
data <- list(
  J = nrow(df_subset),
  G = 5,
  x_bar1 = df_subset[,c("white_percentage", "black_percentage", "hispanic_percentage", "asian_percentage", "other_percentage")],
  y_bar = df_subset[,c("pr1", "pr2", "pr3")]
)
fit <- rstan::sampling(model, data, chains = 2)

# model 5 ----
model <- rstan::stan_model("code/stan/v5_ecological_regression_prop.stan")
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
         pr1 = transmitted/voting_age,
         pr2 = mail_ballots_submitted/transmitted,
         pr3 = rejected/mail_ballots_submitted)
cat(sum(as.integer(df_subset$pr3 > 1)))
data <- list(
  J = nrow(df_subset),
  G = 5,
  x_bar = df_subset[,c("white_percentage", "black_percentage", "hispanic_percentage", "asian_percentage", "other_percentage")],
  y_bar = df_subset[,c("pr1", "pr2", "pr3")]
)
fit <- rstan::sampling(model, data, chains = 2)
# plot
categories <- c("white", "black", "latinx", "asian", "other")
kind <- c("requesting", "submitting", "rejection")
beta <- rstan::extract(fit, pars = "beta")[[1]]
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
  facet_wrap(~kind)





# model 7 ----
model <- rstan::stan_model("code/stan/v7_ecological_regression_prop.stan")
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
         voters_white = turnout[1] * white_count,
         voters_black = turnout[2] * black_count,
         voters_hispanic = turnout[3] * hispanic_count,
         other_count = total_pop - white_count - black_count - hispanic_count,
         voters_other = turnout[4] * other_count,
         white_voteshare = voters_white/(voters_white + voters_black + voters_hispanic + voters_other),
         black_voteshare = voters_black/(voters_white + voters_black + voters_hispanic + voters_other),
         hispanic_voteshare = voters_hispanic/(voters_white + voters_black + voters_hispanic + voters_other),
         other_voteshare = voters_other/(voters_white + voters_black + voters_hispanic + voters_other),
         pr1 = transmitted/(voters_white + voters_black + voters_hispanic + voters_other),
         pr2 = mail_ballots_submitted/transmitted,
         pr3 = rejected/mail_ballots_submitted) %>%
  filter((pr1 >= 0) & (pr1 <= 1))
cat("pr1 out of bounds", sum(as.integer(df_subset$pr1 > 1 | df_subset$pr1 < 0)), "\n",
    "pr2 out of bounds", sum(as.integer(df_subset$pr2 > 1 | df_subset$pr2 < 0)), "\n",
    "pr3 out of bounds", sum(as.integer(df_subset$pr3 > 1   | df_subset$pr3 < 0))
    )
# request
data_requested <- list(
  J = nrow(df_subset),
  G = 4,
  x_bar = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "other_voteshare")],
  y_bar = df_subset[,c("pr1")]
)
fit_requested <- rstan::sampling(model, data_requested, chains = 2)
beta_requested <- rstan::extract(fit_requested, pars = "beta")[[1]]
# submitted
softmax <- function(x) x/sum(x)
beta_requested_mean <- apply(beta_requested, 2, mean)
x_bar_submitted <- data_requested$x_bar
for (j in 1:nrow(df_subset)) x_bar_submitted[j,] <- softmax(data_requested$x_bar[j,] * beta_requested_mean)
data_submitted <- list(
  J = nrow(df_subset),
  G = 4,
  x_bar = x_bar_submitted,
  y_bar = df_subset[,c("pr2")]
)
fit_submitted <- rstan::sampling(model, data_submitted, chains = 2)
beta_submitted <- rstan::extract(fit_submitted, pars = "beta")[[1]]
# rejected
beta_submitted_mean <- apply(beta_submitted, 2, mean)
x_bar_rejected <- data_submitted$x_bar
for (j in 1:nrow(df_subset)) x_bar_rejected[j,] <- softmax(data_submitted$x_bar[j,] * beta_submitted_mean)
data_rejected <- list(
  J = nrow(df_subset),
  G = 4,
  x_bar = x_bar_rejected,
  y_bar = df_subset[,c("pr3")]
)
fit_rejected <- rstan::sampling(model, data_rejected, chains = 2)
beta_rejected <- rstan::extract(fit_rejected, pars = "beta")[[1]]
categories <- c("white", "black", "latinx", "other")
kind <- c("requesting", "submitting", "rejection")
plt_df <- matrix(NA, ncol = 4, nrow = 0)
plt_df <- rbind(plt_df, cbind(categories, apply(beta_requested, MARGIN = 2, mean), apply(beta_requested, MARGIN = 2, sd), kind[1]))
plt_df <- rbind(plt_df, cbind(categories, apply(beta_submitted, MARGIN = 2, mean), apply(beta_submitted, MARGIN = 2, sd), kind[2]))
plt_df <- rbind(plt_df, cbind(categories, apply(beta_rejected, MARGIN = 2, mean), apply(beta_rejected, MARGIN = 2, sd), kind[3]))
plt_df <- as.data.frame(plt_df)
colnames(plt_df) <- c("ethn", "mean_rate", "sd_rate", "kind")
plt_df <- plt_df %>% 
  mutate(mean_rate = round(as.numeric(as.character(mean_rate)), 5),
         sd_rate   = round(as.numeric(as.character(sd_rate)), 5))
ggplot(data = plt_df) + 
  geom_point(aes(x = ethn, y = mean_rate)) + 
  geom_errorbar(aes(x = ethn, y = mean_rate, ymax = mean_rate + sd_rate, ymin = mean_rate - sd_rate)) +
  facet_wrap(~kind, scales = "free")
# ppd checks
## graphs
y_bar <- rstan::extract(fit_rejected, pars = "y_bar_hat")[[1]]
sample_counties <- function(n, df, y_bar, pr_kind, kind){
  N <- nrow(df)
  row_id <- sample(1:N, n, replace = FALSE)
  # y, ybar, name, 
  df_plt <- matrix(NA, nrow = 0, ncol = 4)
  for (i in 1:n){
    df_plt <- rbind(df_plt, cbind(df[row_id[i], 
                                     c("JurisdictionName", pr_kind)], 
                                  y_bar[,row_id[i]], kind))
  }
  df_plt <- as.data.frame(df_plt)
  colnames(df_plt) <- c("name", "pr", "yhat", "kind")
  return(df_plt)
}
df_plt <- sample_counties(24, df_subset, y_bar, "pr3", "rejection")
ggplot() + 
  geom_histogram(data = df_plt, aes(x = yhat)) + 
  geom_vline(df_plt %>% group_by(name) %>% summarize(pr1 = mean(pr)), mapping = aes(xintercept = pr1)) +
  facet_wrap(~name)
y_bar <- rstan::extract(fit_requested, pars = "y_bar_hat")[[1]]
df_plt <- sample_counties(24, df_subset, y_bar, "pr1", "request")
ggplot() + 
  geom_histogram(data = df_plt, aes(x = yhat)) + 
  geom_vline(df_plt %>% group_by(name) %>% summarize(pr1 = mean(pr)), mapping = aes(xintercept = pr1)) +
  facet_wrap(~name)
y_bar <- rstan::extract(fit_submitted, pars = "y_bar_hat")[[1]]
df_plt <- sample_counties(24, df_subset, y_bar, "pr2", "submitted")
ggplot() + 
  geom_histogram(data = df_plt, aes(x = yhat)) + 
  geom_vline(df_plt %>% group_by(name) %>% summarize(pr1 = mean(pr)), mapping = aes(xintercept = pr1)) +
  facet_wrap(~name)
## true v predicted
y_bar_requested <- rstan::extract(fit_requested, pars = "y_bar_hat")[[1]]
requested <- data.frame(y = df_subset$pr1, y_hat = apply(y_bar_requested, MARGIN = 2, mean))
ggplot(data = requested, aes(x = y, y = y_hat)) + 
  geom_point()
## residuals
y_bar_requested <- rstan::extract(fit_requested, pars = "y_bar_hat")[[1]]
requested <- data.frame(y = df_subset$pr1, y_hat = apply(y_bar_requested, MARGIN = 2, mean))
requested <- requested %>% mutate(
  residuals = y - y_hat
) %>% add_column(white_voteshare = df_subset$white_voteshare,
                 black_voteshare = df_subset$black_voteshare)
ggplot(data = requested, aes(x = white_voteshare, y = residuals)) + 
  geom_point()



# model 8 ----
m8 <- rstan::stan_model("code/stan/v8_ecological_regression_prop.stan")
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
         voters_white = turnout[1] * white_count,
         voters_black = turnout[2] * black_count,
         voters_hispanic = turnout[3] * hispanic_count,
         other_count = total_pop - white_count - black_count - hispanic_count,
         voters_other = turnout[4] * other_count,
         white_voteshare = voters_white/(voters_white + voters_black + voters_hispanic + voters_other),
         black_voteshare = voters_black/(voters_white + voters_black + voters_hispanic + voters_other),
         hispanic_voteshare = voters_hispanic/(voters_white + voters_black + voters_hispanic + voters_other),
         other_voteshare = voters_other/(voters_white + voters_black + voters_hispanic + voters_other),
         pr1 = transmitted/(voters_white + voters_black + voters_hispanic + voters_other),
         pr2 = mail_ballots_submitted/transmitted,
         pr3 = rejected/mail_ballots_submitted) %>%
  filter((pr1 >= 0) & (pr1 <= 1))
cat("pr1 out of bounds", sum(as.integer(df_subset$pr1 > 1 | df_subset$pr1 < 0)), "\n",
    "pr2 out of bounds", sum(as.integer(df_subset$pr2 > 1 | df_subset$pr2 < 0)), "\n",
    "pr3 out of bounds", sum(as.integer(df_subset$pr3 > 1   | df_subset$pr3 < 0))
)
req_sub <- ggplot(data = df_subset, aes(x = pr1, y = pr2)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = df_subset, aes(x = pr2, y = pr3)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = df_subset, aes(x = pr1, y = pr3)) + 
  geom_point(size = 0.2)
grid.arrange(req_sub, sub_rej, req_rej)
# request

data_m8 <- list(
  J = nrow(df_subset),
  G = 4,
  xbar1 = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "other_voteshare")],
  ybar = df_subset[,c("pr1", "pr2", "pr3")]
)
fit_m8 <- rstan::sampling(m8, data_m8, chains = 2)
## coef
categories <- c("white", "black", "latinx", "other")
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
## graphs
y_bar <- rstan::extract(fit_m8, pars = "y_bar_hat")[[1]]
sample_counties <- function(n, df, y_bar){
  N <- nrow(df)
  row_id <- sample(1:N, n, replace = FALSE)
  # y, ybar, name, 
  df_plt <- matrix(NA, nrow = 0, ncol = 4)
  kinds <- c("pr1", "pr2", "pr3")
  for (i in 1:n){
    for (j in 1:3){
      add <- cbind(df[row_id[i], 
                          c("JurisdictionName", 
                            kinds[j])], 
                       y_bar[,row_id[i],j], 
                       kinds[j])
      colnames(add) <- c("name", "pr", "yhat", "kind")
      df_plt <- rbind(df_plt, add)
    }
  }
  df_plt <- as.data.frame(df_plt)
  colnames(df_plt) <- c("name", "pr", "yhat", "kind")
  return(df_plt)
}
df_plt <- sample_counties(12, df_subset, y_bar)
ggplot() + 
  geom_histogram(data = df_plt %>% filter(kind == "pr1"), aes(x = yhat)) + 
  geom_vline(df_plt %>% filter(kind == "pr1") %>%
               group_by(name) %>% 
               summarize(pr3 = mean(pr)), mapping = aes(xintercept = pr3)) +
  facet_wrap(~name)
# true v predicted
df_res <- matrix(NA, ncol = 5, nrow = 0)
kind = c("pr1", "pr2", "pr3")
for (j in 1:3){
  y_bar_requested <- rstan::extract(fit_m8, pars = "y_bar_hat")[[1]][,,j]
  add <- cbind(kind[j], df_subset[,kind[j]], apply(y_bar_requested, MARGIN = 2, mean), df_subset$white_voteshare, df_subset$black_voteshare)
  df_res <- rbind(df_res, add)
}
df_res <- as.data.frame(df_res)
colnames(df_res) <- c("kind", "y", "y_hat", "white_voteshare", "black_voteshare")
df_res <- df_res %>% mutate(
  y = as.numeric(as.character(y)),
  y_hat = as.numeric(as.character(y_hat)),
  white_voteshare = as.numeric(as.character(white_voteshare)),
  black_voteshare = as.numeric(as.character(black_voteshare))
)
ggplot(data = df_res, aes(x = y, y = y_hat, color = white_voteshare)) + 
  geom_point(size = 0.1)  +
  geom_abline() +
  facet_wrap(~kind, scales = "free")



# model 9 ----
m9 <- rstan::stan_model("code/stan/v9_ecological_regression_prop.stan")
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
         voters_white = turnout[1] * white_count,
         voters_black = turnout[2] * black_count,
         voters_hispanic = turnout[3] * hispanic_count,
         other_count = total_pop - white_count - black_count - hispanic_count,
         voters_other = turnout[4] * other_count,
         white_voteshare = voters_white/(voters_white + voters_black + voters_hispanic + voters_other),
         black_voteshare = voters_black/(voters_white + voters_black + voters_hispanic + voters_other),
         hispanic_voteshare = voters_hispanic/(voters_white + voters_black + voters_hispanic + voters_other),
         other_voteshare = voters_other/(voters_white + voters_black + voters_hispanic + voters_other),
         pr1 = transmitted/(voters_white + voters_black + voters_hispanic + voters_other),
         pr2 = mail_ballots_submitted/transmitted,
         pr3 = rejected/mail_ballots_submitted) %>%
  filter((pr1 >= 0) & (pr1 <= 1)) %>%
  mutate(group_id = group_indices(., State))

cat("pr1 out of bounds", sum(as.integer(df_subset$pr1 > 1 | df_subset$pr1 < 0)), "\n",
    "pr2 out of bounds", sum(as.integer(df_subset$pr2 > 1 | df_subset$pr2 < 0)), "\n",
    "pr3 out of bounds", sum(as.integer(df_subset$pr3 > 1   | df_subset$pr3 < 0))
)
# request
data_m9 <- list(
  J = nrow(df_subset),
  G = 4,
  S = length(unique(df_subset$group_id)),
  s = df_subset$group_id,
  x_bar = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "other_voteshare")],
  y_bar = df_subset[,c("pr1")]
)
fit_m9 <- rstan::sampling(m9, data_m9, chains = 2)
## coef
coef_df <- matrix(NA, ncol = 4, nrow = 0)
cat <- c("white", "black", "hispanic",'other')
beta <- rstan::extract(fit_m8, pars = "beta")[[1]]
for (i in 1:length(unique(df_subset$group_id))){
  state <- df_subset %>% distinct(State, group_id) %>% filter(group_id == i) %>% select(State)
  for (j in 1:4) {
    coef_df <- rbind(coef_df, 
                   cbind(state, 
                         mean(beta[,i, j]), 
                         sd(beta[,i, j]), cat[j]))
  }
}
coef_df <- as.data.frame(coef_df)
colnames(coef_df) <- c("state", "mean_rate", "sd_rate", "kind")
ggplot(data = coef_df %>% filter(kind == "hispanic") %>% mutate(
  state = factor(state, levels = state[order(mean_rate)])
)) + 
  geom_point(aes(x = state, y = mean_rate)) +
  coord_flip()
  


# model 11----
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
cat("Implied total voters:  ", sum(df_subset[,c("voters_white", "voters_black", "voters_hispanic", "voters_other")]))
cat("pr1 out of bounds", sum(as.integer(df_subset$pr1 > 1 | df_subset$pr1 < 0)), "\n",
    "pr2 out of bounds", sum(as.integer(df_subset$pr2 > 1 | df_subset$pr2 < 0)), "\n",
    "pr3 out of bounds", sum(as.integer(df_subset$pr3 > 1   | df_subset$pr3 < 0))
)
# request
data_m11 <- list(
  J = nrow(df_subset),
  G = 4,
  S = length(unique(df_subset$group_id)),
  s = df_subset$group_id,
  xbar1 = df_subset[,c("white_voteshare", "black_voteshare", "hispanic_voteshare", "other_voteshare")],
  ybar = df_subset[,c("pr1", "pr2", "pr3")]
)
fit_m11_region <- rstan::sampling(m11, data_m11, chains = 4, cores = 4, warmup = 1500, iter = 2250)
write_rds(fit_m11_region, path = "model_fits/fit_m11_region.Rds")
df_subset %>% select(region, group_id) %>% distinct()
# coef
cat <- df_subset %>% select(group_id, region) %>% distinct()
groups <- c("white", "black", "latinx", "other")
kind <- c("requested", "submitted", "rejected")
beta_region <- rstan::extract(fit_m11_region, pars = "beta")[[1]]
# [iter, kind, region, group]
df_region_coef <- matrix(NA, nrow = 0, ncol = 6)
for (i in 1:nrow(cat)){
  for (j in 1:4){
    for (r in 1:3){
      add <- cbind(as.character(cat[cat$group_id == i,"region"]),
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
colnames(df_region_coef) <- c("region", "group", "kind", "mean_rate", "q025", "q075")
rownames(df_region_coef) <- NULL
df_region_coef <- df_region_coef %>%
  mutate(
    mean_rate = as.numeric(as.character(mean_rate)),
    q025 = as.numeric(as.character(q025)),
    q075 = as.numeric(as.character(q075)),
    kind = factor(kind, levels = c("requested", "submitted", "rejected"))
  )
df_mean_rates_region <- df_subset %>%
  group_by(region) %>%
  summarize(
    requested = mean(pr1, na.rm = TRUE),
    submitted = mean(pr2, na.rm = TRUE),
    rejected = mean(pr3, na.rm = TRUE)
  ) %>% pivot_longer(cols = c(requested, submitted, rejected), names_to = "kind", values_to = "mean_rate") %>% 
  add_column(q025 = NA, q075 = NA, group = "average") %>%
  select(region, group, kind, mean_rate, q025, q075)
df_region_coef <- rbind(df_region_coef, df_mean_rates_region)
coef_plot_region <- ggplot(data = df_region_coef, aes(x = region, y = mean_rate, color = group, shape = group)) +
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = region, y = mean_rate, ymin = q025, ymax = q075, color = group, shape = group), 
                position = position_dodge(0.5), width = 0) +
  scale_shape_manual(values=c(16, 16, 16,16,6)) + 
  scale_color_manual(values=alpha(c('red','blue', 'orange', 'green', 'black'), 0.8)) +
  facet_wrap(kind ~ ., scales = "free_y") + 
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90))

df_subset %>%
  pivot_longer(cols = c(pr1, pr2, pr3),
               names_to = 'kind'
  ) %>% select(X, kind)
rates <- df_subset %>%
  pivot_longer(cols = c(pr1, pr2, pr3),
               names_to = 'kind',
               values_to = 'rate'
  ) %>% 
  select(FIPSCode, kind, rate, region)
voters <- df_subset %>%
  pivot_longer(cols = c(white_voteshare, black_voteshare, 
                        hispanic_voteshare, other_voteshare),
               names_to = 'group',
               names_pattern = '(.+)_.+',
               values_to = 'share'
  ) %>% 
  select(FIPSCode, group, share)
combined <- left_join(rates, voters, by = "FIPSCode")
shares_rates <- ggplot(data = combined, aes(x = share, y = rate)) + 
  geom_point(size = 0.5) + 
  facet_grid(kind +group ~ region) + 
  theme_bw()
# voting power 
states <- df_subset %>% distinct(State) %>% pull(State)
n_states <- length(states)
region_crosswalk <- df_subset %>% 
  distinct(State, group_id) %>% 
  pull(group_id)

# voters
# FIPSCode, State, n_absentee_voters, white, black, latino, other,
#                  n_submitted_voters, white, black, latino,other
#                  n_rejected_voters, white, black, latino, other
df_sim <- matrix(NA, nrow = 0, ncol = 21)
for (id in sample(1:3000, 3, replace = FALSE)){
  for (i in 1:nrow(df_subset)) {
    n_absentee_voters = 0
    n_absentee_group = rep(0, 4)
    n_voters = df_subset[i, "voters_count"]
    i_region = df_subset[i, "group_id"]
    n_limit = floor(n_voters * .7)
    while (n_absentee_voters < n_limit) {
      n_group <- df_subset[i, c("voters_white", "voters_black", "voters_hispanic", "voters_other")] - n_absentee_group
      val = sapply(1:4, function(x) rbinom(1, as.integer(n_group[x]), prob = as.numeric(beta_region[id, 1, i_region, x])))
      n_absentee_group = n_absentee_group + val
      n_absentee_voters = sum(n_absentee_group)
    }
    n_submitting_voters = 0
    n_submitting_group = rep(0, 4)
    n_submitting_group = sapply(1:4, function(x) rbinom(1, 
                                            n_absentee_group[x], 
                                            prob = as.numeric(beta_region[id, 2, i_region, x])))
    n_submitting_voters = sum(n_submitting_group)
    n_rejected_voters = 0
    n_rejected_group = rep(0, 4)
    n_rejected_group = sapply(1:4, function(x) rbinom(1, 
                                                      n_submitting_group[x], 
                                                        prob = as.numeric(beta_region[id, 3, i_region, x])))
    n_rejected_voters = sum(n_rejected_group)
    data <- df_subset[i, c("FIPSCode", "State", "voters_white", "voters_black", "voters_hispanic", "voters_other")]
    add <-  c(n_absentee_voters, n_absentee_group, 
      n_submitting_voters, n_submitting_group,
      n_rejected_voters, n_rejected_group)
    df_sim <- rbind(df_sim, c(as.character(data), add))
  }
}
df_sim <- as.data.frame(df_sim)
df_sim <- turn_numeric(df_sim, 2)
colnames(df_sim) <- c("FIPSCode", "State", "voters_white", "voters_black", "voters_hispanic", "voters_other",
                      "n_requested", "n_requested_white", "n_requested_black", "n_requested_hispanic", "n_requested_other",
                      "n_submitted", "n_submitted_white", "n_submitted_black", "n_submitted_hispanic", "n_submitted_other",
                      "n_rejected", "n_rejected_white", "n_rejected_black", "n_rejected_hispanic", "n_rejected_other")
write_rds(df_sim, path = "model_fits/fit_m11_region_simulated_70percent.Rds")
# simulation plots
df_sim <- read_rds(path = "model_fits/fit_m11_region_simulated_70percent.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup() %>% 
  group_by(State) %>%
  summarize_all(list(mean = mean, sd = sd))
## black ----
plt_n_black_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_black_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_black_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_black_sum_mean + n_rejected_black_sum_sd,
                    ymin = n_rejected_black_sum_mean - n_rejected_black_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Black absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/m11_", Sys.Date(), "_n_rejected_black_by_state.jpeg"), plt_n_black_rejected)
## hispanic ----
plt_n_latinx_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_hispanic_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_hispanic_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_hispanic_sum_mean + n_rejected_hispanic_sum_sd,
                    ymin = n_rejected_hispanic_sum_mean - n_rejected_hispanic_sum_sd), width = 0) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected Latinx absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/m11_", Sys.Date(), "_n_rejected_latinx_by_state.jpeg"), plt_n_latinx_rejected)
## white ----
require(scales)
plt_n_white_rejected <- ggplot(data = df_sim %>% mutate(State = factor(State, levels = State[order(n_rejected_white_sum_mean)]))) + 
  geom_point(aes(x = State, y = n_rejected_white_sum_mean)) + 
  geom_errorbar(aes(x = State, ymax = n_rejected_white_sum_mean + n_rejected_white_sum_sd,
                    ymin = n_rejected_white_sum_mean - n_rejected_white_sum_sd), width = 0) +
  scale_y_continuous(labels = comma) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank()) + 
  labs(y = "Rejected absentee votes", title = "Rejected white absentee ballots (70% of 2016 VBM)")
ggsave(paste0("plots/m11_", Sys.Date(), "_n_rejected_white_by_state.jpeg"), plt_n_white_rejected)
## nation ----
df_sim <- read_rds(path = "model_fits/fit_m11_region_simulated_70percent.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(sim) %>% select(-State) %>%
  summarise_all(list(sum = sum)) %>% 
  ungroup() %>% 
  summarize_all(list(mean = mean, sd = sd))

rejected_mean <- df_sim %>% pivot_longer(cols = c(n_rejected_white_sum_mean, n_rejected_black_sum_mean,
                                 n_rejected_hispanic_sum_mean, n_rejected_other_sum_mean),
                        names_pattern = "n_(.+)_sum_mean",
                        names_to = "kind", values_to = "mean") %>%
  select(kind, mean)
rejected_sd <- df_sim %>% pivot_longer(cols = c(n_rejected_white_sum_sd, n_rejected_black_sum_sd,
                                 n_rejected_hispanic_sum_sd, n_rejected_other_sum_sd),
                        names_pattern = "n_(.+)_sum_sd",
                        names_to = "kind", values_to = "sd") %>%
  select(kind, sd)
rejected <- merge(rejected_mean, rejected_sd, by = "kind")
submitted_mean <- df_sim %>% pivot_longer(cols = c(n_submitted_white_sum_mean, n_submitted_black_sum_mean,
                                                  n_submitted_hispanic_sum_mean, n_submitted_other_sum_mean),
                                         names_pattern = "n_(.+)_sum_mean",
                                         names_to = "kind", values_to = "mean") %>%
  select(kind, mean)
submitted_sd <- df_sim %>% pivot_longer(cols = c(n_submitted_white_sum_sd, n_submitted_black_sum_sd,
                                                n_submitted_hispanic_sum_sd, n_submitted_other_sum_sd),
                                       names_pattern = "n_(.+)_sum_sd",
                                       names_to = "kind", values_to = "sd") %>%
  select(kind, sd)
submitted <- merge(submitted_mean, submitted_sd, by = "kind")

requested_mean <- df_sim %>% pivot_longer(cols = c(n_requested_white_sum_mean, n_requested_black_sum_mean,
                                                  n_requested_hispanic_sum_mean, n_requested_other_sum_mean),
                                         names_pattern = "n_(.+)_sum_mean",
                                         names_to = "kind", values_to = "mean") %>%
  select(kind, mean)
requested_sd <- df_sim %>% pivot_longer(cols = c(n_requested_white_sum_sd, n_requested_black_sum_sd,
                                                n_requested_hispanic_sum_sd, n_requested_other_sum_sd),
                                       names_pattern = "n_(.+)_sum_sd",
                                       names_to = "kind", values_to = "sd") %>%
  select(kind, sd)
requested <- merge(requested_mean, requested_sd, by = "kind")
national_sim <- rbind(requested, submitted, rejected) %>% separate(kind, c("kind", "group")) %>%
  as_tibble() %>% mutate(kind = factor(kind, levels = c("requested", "submitted", "rejected")))
plt_national <- ggplot(data = national_sim) + 
  geom_point(aes(x = group, y = mean)) + 
  geom_errorbar(aes(x = group, y = mean, ymax = mean + sd, ymin = mean - sd), width = 0) + 
  scale_y_continuous(labels = comma) + 
  facet_wrap(~kind, scale = "free") + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  labs(y = "Rejected ballots", title = "N of rejected ballots (70% VBM)")
ggsave(paste0("plots/m11_", Sys.Date(), "_n_rejected_national.jpeg"), plt_national)

  
  
  

## rates ----
df_sim <- read_rds(path = "model_fits/fit_m11_region_simulated_70percent.Rds")
df_sim <- df_sim %>% group_by(FIPSCode) %>% mutate(sim = sequence(n()))
df_sim <- df_sim %>% group_by(State, sim) %>% 
  summarise_all(list(sum = sum)) %>% 
  ungroup()
df_shares <- df_sim %>% 
  transmute(
    State = State,
    sim = sim,
    voters_sum = voters_white_sum + voters_black_sum + voters_hispanic_sum + voters_other_sum,
    share_voters_white = voters_white_sum / voters_sum,
    share_voters_black = voters_black_sum / voters_sum,
    share_voters_hispanic = voters_hispanic_sum / voters_sum,
    share_voters_other = voters_other_sum / voters_sum,
    share_requested_white = n_requested_white_sum / n_requested_sum,
    share_requested_black = n_requested_black_sum / n_requested_sum,
    share_requested_hispanic = n_requested_hispanic_sum / n_requested_sum,
    share_requested_other = n_requested_other_sum / n_requested_sum,
    share_submitted_white = n_submitted_white_sum / n_submitted_sum,
    share_submitted_black = n_submitted_black_sum / n_submitted_sum,
    share_submitted_hispanic = n_submitted_hispanic_sum / n_submitted_sum,
    share_submitted_other = n_submitted_other_sum / n_submitted_sum,
    share_rejected_white = n_rejected_white_sum / n_rejected_sum,
    share_rejected_black = n_rejected_black_sum / n_rejected_sum,
    share_rejected_hispanic = n_rejected_hispanic_sum / n_rejected_sum,
    share_rejected_other = n_rejected_other_sum / n_rejected_sum
  ) %>% 
  select(-voters_sum) %>%
  pivot_longer(cols = c(-State, -sim), 
               names_pattern = "share_(.+)_(.+)", 
               names_to = c("kind", "group"), 
               values_to = "share") %>%
  group_by(State, kind, group) %>% 
  select(-sim) %>%
  summarize_all(list(mean = mean, sd = sd)) %>%
  ungroup()
rates_white <- ggplot(data = df_shares %>% 
         filter(group == 'white') %>%
         mutate(State = factor(State, levels = df_shares %>% 
                                 filter(kind == 'voters', group == "white") %>% 
                                 arrange(mean) %>% 
                                 pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw() + 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "white")
rates_black <- ggplot(data = df_shares %>% 
                        filter(group == 'black') %>%
                        mutate(State = factor(State, levels = df_shares %>% 
                                                filter(kind == 'voters', group == "black") %>% 
                                                arrange(mean) %>% 
                                                pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "black")
rates_hispanic <- ggplot(data = df_shares %>% 
                        filter(group == 'hispanic') %>%
                        mutate(State = factor(State, levels = df_shares %>% 
                                                filter(kind == 'voters', group == "hispanic") %>% 
                                                arrange(mean) %>% 
                                                pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "none", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "latinx")
rates_other <- ggplot(data = df_shares %>% 
                        filter(group == 'other') %>%
                        mutate(State = factor(State, levels = df_shares %>% 
                                                filter(kind == 'voters', group == "other") %>% 
                                                arrange(mean) %>% 
                                                pull(State)))) +
  geom_point(aes(x = State, y = mean, color = kind), position = position_dodge(0.5)) + 
  geom_errorbar(aes(x = State, ymin = mean - sd, ymax = mean + sd, color = kind), position = position_dodge(0.5)) + 
  theme_bw()+ 
  theme(axis.title = element_blank(), legend.position = "right", axis.text.x = element_text(angle = 90)) + 
  labs("Share", title = "other")
plt_rates <- grid.arrange(rates_white, rates_black, rates_hispanic, rates_other, ncol = 2)
ggsave(paste0("plots/m11_", Sys.Date(), "_group_shares_by_state.jpeg"), plt_rates, width = 14, height = 8)
