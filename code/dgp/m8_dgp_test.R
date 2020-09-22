# libraries
library(rstan)
library(tidyverse)
library(DirichletReg)
library(gridExtra)
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
beta_true[1,] <- runif(G, min = 0.03, max =0.17)
beta_true[2,] <- runif(G, min = 0.87, max =0.96)
beta_true[3,] <- runif(G, min = 0.001, max =0.022)
requested <- submitted <- rejected <- rep(NA, N)
epsilon <- matrix(N, ncol = 3, nrow = N)
epsilon[, 1] <- rnorm(N, 0, 0.005)
epsilon[, 2] <- rnorm(N, 0, 0.005)
epsilon[, 3] <- rnorm(N, 0, 0.005)
eta <- t(beta_true[,g]) + epsilon
eta <- ifelse(eta > 1, 1, eta)
eta <- ifelse(eta <0, 0, eta)

for (q in 1:G){
  requested[g == q] <- rbinom(sum(as.integer(g == q)), 1, prob = eta[g == q, 1])
  submitted[g == q] <- ifelse(requested[g == q] == 1, 
                              rbinom(sum(as.integer(g == q)), 1, prob = eta[g == q, 2]), 0)
  rejected[g == q]  <- ifelse(submitted[g == q] == 1, rbinom(sum(as.integer(g == q)), 1, 
                                                             prob = eta[g == q, 3]), 0)
}
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
  pr1 = sum(requested)/n(),
  pr2 = sum(submitted)/sum(requested),
  pr3 = sum(rejected)/sum(submitted),
  N = n()
) %>% rename(
  requested = pr1,
  submitted = pr2, 
  rejected = pr3
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
