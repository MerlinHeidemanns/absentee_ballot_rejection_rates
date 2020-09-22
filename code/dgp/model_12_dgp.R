# libraries
library(rstan)
library(tidyverse)
library(DirichletReg)
library(gridExtra)
library(MASS)
library(lqmm)
# notes
# 
#//convince somebody
##//jonathan nagler
#//ant salverhar
# dgp
N <- 55e6
J <- 1000
G <- 4
g_pr <- rdirichlet(1, rep(1, G))
j_pr <- rdirichlet(4, rep(10, J))
g <- sample(seq(1, G), N, prob = g_pr, replace = TRUE)
beta_true <- matrix(NA, ncol = 4, nrow = 3)
make_cov_matrix <- function(n, rho_12, rho_13, rho_23, sigma){
  tmp <- matrix(sigma^2, ncol = n, nrow = n)
  tmp[1, 2] <- tmp[2, 1] <- sigma^2 * rho_12
  tmp[1, 3] <- tmp[3, 1] <- sigma^2 * rho_13
  tmp[2, 3] <- tmp[3, 2] <- sigma^2 * rho_23
  return(tmp)
}
Sigma <- make_cov_matrix(3, 0.5, 0.1, -0.5, 0.1)
is.positive.definite(Sigma)
mu <- matrix(NA, ncol = 4, nrow = 3)
mu[1,] <- runif(G, min = 0.03, max =0.17)
mu[2,] <- runif(G, min = 0.87, max =0.96)
mu[3,] <- runif(G, min = 0.001, max =0.022) 
beta_true <- matrix(NA, ncol = 3, nrow = N)
for (i in 1:G) beta_true[g == i] <- MASS::mvrnorm(sum(g == i), mu[, i], Sigma)
beta_true[beta_true > 1] <- 1
beta_true[beta_true < 0] <- 0
requested <- submitted <- rejected <- rep(NA, N)
for (q in 1:G){
  requested[g == q] <- rbinom(sum(as.integer(g == q)), 1, prob = beta_true[g == q, 1])
  submitted[g == q] <- ifelse(requested[g == q] == 1, 
                              rbinom(sum(as.integer(g == q)), 1, prob = beta_true[g == q, 2]), 0)
  rejected[g == q]  <- ifelse(submitted[g == q] == 1, rbinom(sum(as.integer(g == q)), 1, 
                                                             prob = beta_true[g == q, 3]), 0)
}
cat(sum(requested), sum(is.na(requested)), 
    sum(submitted),sum(is.na(submitted)), 
    sum(rejected),  sum(is.na(rejected)))
j <- rep(NA, N)
for (q in 1:G){
  j[g == q] <- sample(seq(1, J), size = length(j[g == q]), prob = j_pr[q,], replace = TRUE)
}
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
req_sub <- ggplot(data = df, aes(x = requested, y = submitted)) + 
  geom_point(size = 0.2)
sub_rej <- ggplot(data = df, aes(x = submitted, y = rejected)) + 
  geom_point(size = 0.2)
req_rej <- ggplot(data = df, aes(x = requested, y = rejected)) + 
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
# compare
beta_hat <- rstan::extract(fit_m8, pars = "beta")[[1]]
beta_mean <- apply(beta, MARGIN = c(2, 3), mean)
beta_sd <- apply(beta, MARGIN = c(2, 3), sd)
print(beta_mean - t(beta_true))
print(beta_sd)