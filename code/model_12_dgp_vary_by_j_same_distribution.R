# libraries
library(rstan)
library(tidyverse)
library(DirichletReg)
library(gridExtra)
library(MASS)
library(lqmm)
# notes
# parameters vary by j but come from the same distribution
# dgp
N <- 55e6
J <- 1000
G <- 4
g_pr <- rdirichlet(1, rep(1, G))
j_pr <- rdirichlet(4, rep(10, J))
g <- sample(seq(1, G), N, prob = g_pr, replace = TRUE)
j <- rep(NA, N)
for (q in 1:G){
  j[g == q] <- sample(seq(1, J), size = length(j[g == q]), prob = j_pr[q,], replace = TRUE)
}
mu <- matrix(NA, ncol = 4, nrow = 3)
mu[1,] <- runif(G, min = 0.03, max =0.17)
mu[2,] <- runif(G, min = 0.87, max =0.96)
mu[3,] <- runif(G, min = 0.001, max =0.022) 
beta_true <- array(NA, dim = c(3, G, J))
for (i_ in 1:G) for (j_ in 1:3) beta_true[j_, i_, ] <- rnorm(J, mu[j_, i_], sd = 0.02)
beta_true[beta_true > 1] <- 1
beta_true[beta_true < 0] <- 0
requested <- submitted <- rejected <- rep(NA, N)
for (q in 1:G){
  requested[g == q] <- rbinom(sum(as.integer(g == q)), 1, prob = beta_true[1, q,j[g == q]])
  submitted[g == q] <- ifelse(requested[g == q] == 1, 
                              rbinom(sum(as.integer(g == q)), 1, prob = beta_true[2, q,j[g == q]]), 0)
  rejected[g == q]  <- ifelse(submitted[g == q] == 1, rbinom(sum(as.integer(g == q)), 1, 
                                                             prob = beta_true[3, q, j[g == q]]), 0)
}
cat(sum(requested), sum(is.na(requested)), 
    sum(submitted),sum(is.na(submitted)), 
    sum(rejected),  sum(is.na(rejected)))
df <- data.frame(
  g = g, 
  requested = requested, 
  submitted = submitted,
  rejected = rejected,
  j = j
) %>% mutate(
  g1 = ifelse(g == 1, 1, 0),
  g2 = ifelse(g == 2, 1, 0),
  g3 = ifelse(g == 3, 1, 0),
  g4 = ifelse(g == 4, 1, 0)
)
df <- df %>% group_by(j) %>%
  summarize(
    g1 = mean(g1),
    g2 = mean(g2),
    g3 = mean(g3),
    g4 = mean(g4),
    N = n(),
    n_requested = sum(requested),
    n_submitted = sum(submitted),
    n_rejected = sum(rejected)
  ) %>%
  mutate(
    requested = n_requested/N,
    submitted = n_submitted/n_requested,
    rejected  = n_rejected/n_submitted
  )
req_sub <- ggplot(data = df, aes(x = requested, y = submitted, color = j)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = df, aes(x = submitted, y = rejected, color = j)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = df, aes(x = requested, y = rejected, color = j)) + 
  geom_point(size = 0.2)
grid.arrange(req_sub, sub_rej, req_rej)
# model
m8 <- rstan::stan_model("code/stan/v8_ecological_regression_prop.stan")
data_m8 <- list(
  J = nrow(df),
  G = 4,
  xbar1 = df[,c("g1", "g2", "g3", "g4")],
  ybar = df[,c("requested", "submitted", "rejected")]
)
fit_m8 <- rstan::sampling(m8, data_m8, chains = 2, iter = 4000, warmup = 2000)
