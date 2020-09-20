data {
  int J;
  int G;
  int S;
  int s[J];
  matrix[J, G] xbar1;
  matrix[J, 3] ybar;
}
transformed data {
  vector[G] ones_column = rep_vector(1, G);
  row_vector[G] ones_row = rep_vector(1, G)';
}
parameters {
  matrix<lower = 0, upper = 1>[G,2] beta_mean;
  matrix<lower = 0>[G,2] beta_sigma;
  matrix<lower = 0, upper = 1>[S,G] beta[2];
  vector<lower = 0>[2] sigma;
}
model {
  matrix[J, G] xbar2;
  xbar2 = (xbar1 .* beta[1, s]);
  xbar2 = xbar2 ./ (xbar2 * ones_column * ones_row);
  for (j in 1:2) for (g in 1:G) beta[j, ,g] ~ normal(beta_mean[g, j], beta_sigma[g, j]);
  to_vector(beta_mean) ~ normal(0, 0.5);
  to_vector(beta_sigma) ~ normal(0, 0.25);
  target += normal_lpdf(ybar[:,1] | rows_dot_product(xbar1, beta[1,s]), sigma[1]);
  target += normal_lpdf(ybar[:,2] | rows_dot_product(xbar2, beta[2,s]), sigma[2]);
}
