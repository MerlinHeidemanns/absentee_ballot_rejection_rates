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
  matrix<lower = 0, upper = 1>[G,3] beta_mean;
  matrix<lower = 0>[G,3] beta_sigma;
  matrix<lower = 0, upper = 1>[S,G] beta[3];
  vector<lower = 0>[3] sigma;
}
model {
  matrix[J, G] xbar2;
  matrix[J, G] xbar3;
  xbar2 = (xbar1 .* beta[1, s]);
  xbar2 = xbar2 ./ (xbar2 * ones_column * ones_row);
  xbar3 = (xbar2 .* beta[2, s]);
  xbar3 = xbar3 ./ (xbar3 * ones_column * ones_row);
  for (j in 1:2) for (g in 1:G) beta[j, ,g] ~ normal(beta_mean[g, j], beta_sigma[g, j]);
  to_vector(beta_mean) ~ normal_lpdf(0, 0.5);
  to_vector(beta_sigma) ~ normal_lpdf(0.25, 0.25);
  ybar[:,1] ~ normal(rows_dot_product(xbar1, beta[1,s]), sigma[1]);
  ybar[:,2] ~ normal(rows_dot_product(xbar2, beta[2,s]), sigma[2]);
  ybar[:,3] ~ normal(rows_dot_product(xbar3, beta[3,s]), sigma[3]);
}
