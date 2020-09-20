data {
  int J;
  int G;
  int S;
  int s[J];
  matrix[J, G] x_bar;
  vector[J] y_bar;
}
parameters {
  vector<lower = 0, upper = 1>[G] beta_mean;
  vector<lower = 0>[G] beta_sigma;
  matrix<lower = 0, upper = 1>[S, G] beta;
  real<lower = 0> sigma;
}
model {
  target += normal_lpdf(beta_mean | 0.5, 0.25);
  target += normal_lpdf(beta_sigma | 0.5, 0.25);
  for (g in 1:G) target += normal_lpdf(beta[,g] | beta_mean[g], beta_sigma[g]);
  target += normal_lpdf(sigma | 0, 2);
  target += normal_lpdf(y_bar | rows_dot_product(beta[s], x_bar), sigma);
}
generated quantities {
  vector[J] y_bar_hat;
  y_bar_hat = to_vector(normal_rng(rows_dot_product(beta[s], x_bar), sigma));
}

