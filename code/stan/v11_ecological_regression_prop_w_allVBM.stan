data {
  // Stack those for which x1 matters
  // Add x2 from below
  // presort in R
  // Oregon last
  // Haiti, Utah, etc before
  int J[3];
  int G;
  int S[3];
  int s1[J[1]];
  int s2[J[1] + J[2]];
  int s3[J[1] + J[2] + J[3]];
  matrix[J[1], G] xbar1;
  matrix[J[2], G] xbar2_add;
  matrix[J[3], G] xbar3_add;
  vector[J[1]] ybar1;
  vector[J[1] + J[2]] ybar2;
  vector[J[1] + J[2] + J[3]] ybar3;
}
transformed data {
  vector[G] ones_column = rep_vector(1, G);
  row_vector[G] ones_row = rep_vector(1, G)';
}
parameters {
  matrix<lower = 0, upper = 1>[G,3] beta_mean;
  matrix<lower = 0>[G,3] beta_sigma;
  matrix<lower = 0, upper = 1>[S[1],G] beta1;
  matrix<lower = 0, upper = 1>[S[2],G] beta2;
  matrix<lower = 0, upper = 1>[S[3],G] beta3;
  vector<lower = 0>[3] sigma;
}
model {
  matrix[J[1], G] xbar2;
  matrix[J[1] + J[2], G] xba2_added;
  matrix[J[1] + J[2], G] xbar3;
  xbar2 = (xbar1 .* beta1[s1]);
  xbar2 = xbar2 ./ (xbar2 * ones_column * ones_row);
  xba2_added = append_row(xbar2, xbar2_add);
  xbar3 = (xba2_added .* beta2[s2]);
  xbar3 = xbar3 ./ (xbar3 * ones_column * ones_row);
  for (g in 1:G){
    beta1[ ,g] ~ normal(beta_mean[g, 1], beta_sigma[g, 1]);
    beta2[ ,g] ~ normal(beta_mean[g, 2], beta_sigma[g, 2]);
    beta3[ ,g] ~ normal(beta_mean[g, 3], beta_sigma[g, 3]);
  }
  to_vector(beta_mean) ~ normal(0, 0.5);
  to_vector(beta_sigma) ~ normal(0, 0.25);
  sigma ~ normal(0, 1);
  ybar1 ~ normal(rows_dot_product(xbar1, beta1[s1]), sigma[1]);
  ybar2 ~ normal(rows_dot_product(xba2_added, beta2[s2]), sigma[2]);
  ybar3 ~ normal(rows_dot_product(append_row(xbar3, xbar3_add), beta3[s3]), sigma[3]);
}
