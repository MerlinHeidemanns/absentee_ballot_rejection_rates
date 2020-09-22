# libraries
library(rstan)
library(tidyverse)
library(DirichletReg)
library(gridExtra)

# dgp
N <- 55e6
J <- 1000
S <- 8
G <- 4
g_pr <- DirichletReg::rdirichlet(1, rep(1, G))
j_pr <- rdirichlet(4, rep(10, J))
g <- sample(seq(1, G), N, prob = g_pr, replace = TRUE)
j <- rep(NA, N)
for (q in 1:G){
  j[g == q] <- sample(seq(1, J), size = sum(g == q), prob = j_pr[q,], replace = TRUE)
}
s_J_pr <- rdirichlet(1, rep(1, S))
s_J <- sample(1:S, size = J, replace = TRUE, prob = s_J_pr)
beta_mean_true <- matrix(NA, ncol = 4, nrow = 3)
beta_mean_true[1,] <- runif(G, min = 0.03, max =0.17)
beta_mean_true[2,] <- runif(G, min = 0.87, max =0.96)
beta_mean_true[3,] <- runif(G, min = 0.001, max =0.022)
beta_sd_true <- abs(matrix(rnorm(G * 3, mean = 0, sd =0.01), ncol = 4, nrow = 3))
beta_true <- array(NA, dim=c(S, G, 3))
for (i in 1:3){
  for (q in 1:G){
    beta_true[,q, i] = abs(rnorm(S, mean = beta_mean_true[i, q], sd = beta_sd_true[i, q]))
  }
}
# requesting, submitting, rejecting
requested <- submitted <- rejected <- rep(NA, N)
for (q in 1:G){
  requested[g == q] <- rbinom(sum(as.integer(g == q)), 1, prob = beta_true[s_J[j][g == q],q,1])
  submitted[g == q] <- ifelse(requested[g == q] == 1, 
                              rbinom(sum(as.integer(g == q)), 1, prob = beta_true[s_J[j][g == q],q,2]), 0)
  rejected[g == q]  <- ifelse(submitted[g == q] == 1, rbinom(sum(as.integer(g == q)), 1, 
                                                             prob = beta_true[s_J[j][g == q],q,3]), 0)
}
df <- data.frame(
  g = g, 
  requested = requested, 
  submitted = submitted,
  rejected = rejected,
  j = j,
  s = s_J[j]
) %>% mutate(
  g1 = ifelse(g == 1, 1, 0),
  g2 = ifelse(g == 2, 1, 0),
  g3 = ifelse(g == 3, 1, 0),
  g4 = ifelse(g == 4, 1, 0)
)
df <- df %>% group_by(s, j) %>%
  summarize(
    g1 = mean(g1),
    g2 = mean(g2),
    g3 = mean(g3),
    g4 = mean(g4),
    pr1 = sum(requested)/n(),
    pr2 = sum(submitted)/sum(requested),
    pr3 = sum(rejected)/sum(submitted),
    N = n()
  ) %>% rename(
    requested = pr1,
    submitted = pr2, 
    rejected = pr3
  )

size = 0.7
req_sub <- ggplot(data = df, aes(x = requested, y = submitted, color = as.factor(s))) + 
  geom_point(size = size)
sub_rej <- ggplot(data = df, aes(x = submitted, y = rejected, color = as.factor(s))) + 
  geom_point(size = size)
req_rej <- ggplot(data = df, aes(x = requested, y = rejected, color = as.factor(s))) + 
  geom_point(size = size)
grid.arrange(req_sub, sub_rej, req_rej)
# model
m11 <- rstan::stan_model("code/stan/v11_ecological_regression_prop.stan")
data_m11 <- list(
  J = nrow(df),
  G = 4,
  S = S,
  s = df$s,
  xbar1 = df[,c("g1", "g2", "g3", "g4")],
  ybar = df[,c("requested", "submitted", "rejected")]
)
fit_m11 <- rstan::sampling(m11, data = data_m11, chains = 2)
# can recover parameters

