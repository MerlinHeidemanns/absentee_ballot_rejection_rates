data {
  int J;
  vector[J] xbar_j1;
  vector[J] xbar_j2;
  int trials[J];
  int counts[J];
}
parameters {
  vector[2] beta;
}
model {
  target += normal_lpdf(beta | 0, 1);
  target += binomial_logit_lpmf(counts | trials, beta[1] * xbar_j1 + beta[2] * xbar_j2);
}
