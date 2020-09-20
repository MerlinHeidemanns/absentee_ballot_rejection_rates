data {
  int J;
  int G;
  matrix[J, G] xbar;
  int trials[J];
  int counts[J];
}
parameters {
  vector[G] beta;
}
model {
  target += normal_lpdf(beta | 0, 1);
  target += binomial_logit_lpmf(counts | trials, xbar * beta);
}
