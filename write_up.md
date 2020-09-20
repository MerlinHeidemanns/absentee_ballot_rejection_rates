# Estimating the Expected Number of Rejected Ballots

The COVID-19 pandemic in 2020 in the U.S. has raised the spectre of absentee voting/vote-by-mail (VBM) as a safe alternative to voting in person. Yet, while safer from a public health point of view, absentee voting is less safe when it comes to having one's voice heard. Support in casting one's ballot is lower compared to voting in person and reasons for having one's absentee ballot rejected are numerous ranging from a missing witness to the use of a non-official envelope. But beyond the rejection rate being higher generally, VBM also represents an equity problem because rejection rates can vary widely across socio-demographic groups. A study of voting lists in Florida found .... Another study from an election in LA and another from Georgia found similar results showing that first-time voters and those belonging to a minority group generally experience higher rejection rates. While these groups are also usually less likely to vote absentee which diminishes the practical relevance of the issue, in 2020 they may vote at far higher rates absentee than before.

## The Process

Having one's absentee ballot rejected requires three steps. First, the individual has to request an absentee ballot. In some states, all voting age citizens are being sent an absentee ballot such that this step becomes irrelevant. Second, the individual has to submit their absentee ballot. After receiving their absentee ballot, the individual can still vote in person -- early or on Election Day -- or decide not to vote. Third, an election official has to decide whether to reject the mailed-in ballot. A plethora of reasons exist to have a ballot rejected only one of which is the ballot arriving too late. 

Then, the process consists of three steps. If we want to know the rejection rate we care about the proportion of submitted absentee votes within a however defined group that gets rejected. Let 
$$
\theta_{\text{rejected}, i} \in [0, 1]
$$
be that rate for group $i$.

If one were to simulate this process, the R code would look like this:

```R
N <- 1e7
G <- 4
g_pr <- rdirichlet(1, rep(1, G))
g <- sample(seq(1, G), N, prob = g_pr, replace = TRUE)
beta_true <- matrix(NA, ncol = 4, nrow = 3)
beta_true[1,] <- runif(G, min = 0.03, max =0.17)
beta_true[2,] <- runif(G, min = 0.87, max =0.96)
beta_true[3,] <- runif(G, min = 0.001, max =0.022)
eta <- t(beta_true[,g])
requested <- submitted <- rejected <- rep(NA, N)
for (q in 1:G){
  requested[g == q] <- rbinom(sum(as.integer(g == q)), 1, prob = eta[g == q, 1])
  submitted[g == q] <- ifelse(requested[g == q] == 1, 
                              rbinom(sum(as.integer(g == q)), 1, prob = eta[g == q, 2]), 0)
  rejected[g == q]  <- ifelse(submitted[g == q] == 1, rbinom(sum(as.integer(g == q)), 1, 
                                                             prob = eta[g == q, 3]), 0)
}
```

Individuals are assigned with probability to different groups and request, submit, and have their ballot rejected with certain probabilities. We want to know `beta_true[3,]`, i.e. the vector of rejection rates by group.



## Estimation

How to estimate $\theta_{\text{rejected}, i}$? We know that  $\theta_{\text{rejected}, i}$ is a proportion and that any data $x$ will follow a Binomial distribution. Using Bayes rule, we can estimate $\theta_{\text{rejected}, i}$ as:
$$
p(\theta|x) \propto p(x|\theta)p(\theta)
$$
where $p(\theta|x)$ and $p(\theta)$ are Beta distributions (posterior and prior) and $p(x|\theta)$ is a Binomial likelihood. The beta distribution has two parameters $\alpha, \beta$, whereas the the binomial distribution has $\theta$ as its unknown parameter with $N, x$ considered known/data. Then
$$
p(\theta | \alpha, \beta, N, x) \propto p(x, N | \theta)p(\theta | \alpha, \beta)
$$
Such that the posterior distribution of $\theta$ is $\text{Beta}(\alpha + x, N - x + \beta)$ due to conjugacy.

The problem associated with 



### Artifacts

#### Very high request rate for Whites

Some states feature state-wide vote-by-mail such that everyone receives an absentee ballot. These states are also overwhelmingly white and should not be pooled with the other states.

### Correlation across layers

We might be concerned that the individual probabilities to request, submit, and have one's ballot rejected are correlated. One's unlikeliness to request an absentee ballot or to submit it may imply a higher probability to see one's ballot rejected whereby the latter affects the former. 

Modifying the DGP such that probabilities are drawn from a multivariate normal,

```R
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
```

the unmodified model can recover the $\mu$ parameters just fine.

### No constancy but independently drawn from the same distribution

For ecological regression to provide valid inference, we assume constancy s.t. $\beta = \beta_{j} \forall j$, i.e. that the effect size doesn't vary by geographical area. By simulation we can show that values can be independently drawn from the same distribution for each $j$ s.t. we can still recover the mean of that distribution. The simulation corresponds to the code snippet below

```R
mu <- matrix(NA, ncol = 4, nrow = 3)
mu[1,] <- runif(G, min = 0.03, max =0.17)
mu[2,] <- runif(G, min = 0.87, max =0.96)
mu[3,] <- runif(G, min = 0.001, max =0.022) 
beta_true <- array(NA, dim = c(3, G, J))
for (i_ in 1:G) for (j_ in 1:3) beta_true[j_, i_, ] <- rnorm(J, mu[j_, i_], sd = 0.02) 
```



