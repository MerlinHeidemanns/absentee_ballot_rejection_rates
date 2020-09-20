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
  matrix[S,G] raw_beta[3];
  vector<lower = 0>[3] sigma;
}
transformed parameters {
  matrix[S, G] beta[3];
  for (g in 1:G){
    for (j in 1:3) {
      beta[j,,g] = beta_mean[g, j] + raw_beta[j, ,g] * beta_sigma[g, j];
    }
  }  
}
model {
  matrix[J, G] xbar2;
  matrix[J, G] xbar3;
  xbar2 = (xbar1 .* beta[1, s]);
  xbar2 = xbar2 ./ (xbar2 * ones_column * ones_row);
  xbar3 = (xbar2 .* beta[2, s]);
  xbar3 = xbar3 ./ (xbar3 * ones_column * ones_row);
  for (j in 1:3) to_vector(raw_beta[j]) ~ std_normal();
  to_vector(beta_mean) ~ normal_lpdf(0, 0.5);
  to_vector(beta_sigma) ~ normal_lpdf(0.25, 0.25);
  target += normal_lpdf(ybar[:,1] | rows_dot_product(xbar1, beta[1,s]), sigma[1]);
  target += normal_lpdf(ybar[:,2] | rows_dot_product(xbar2, beta[2,s]), sigma[2]);
  target += normal_lpdf(ybar[:,3] | rows_dot_product(xbar3, beta[3,s]), sigma[3]);
}
//generated quantities {
//  matrix[J, G] xbar2;
//  matrix[J, G] xbar3;
//  matrix[S, G] beta[3];
//  for (g in 1:G){
//    for (j in 1:3) {
//      beta[j,,g] = beta_mean[g, j] + raw_beta[j, ,g] * beta_sigma[g, j];
//    }
//  }  
//  xbar2 = (xbar1 .* beta[1, s]);
//  xbar2 = xbar2 ./ (xbar2 * ones_column * ones_row);
//  xbar3 = (xbar2 .* beta[2, s]);
//  xbar3 = xbar3 ./ (xbar3 * ones_column * ones_row);
//  matrix[J, 3] y_bar_hat;
//  y_bar_hat[,1] = to_vector(normal_rng(rows_dot_product(xbar1, beta[1,s]), sigma[1]));
//  y_bar_hat[,2] = to_vector(normal_rng(rows_dot_product(xbar2, beta[2,s]), sigma[2]));
//  y_bar_hat[,3] = to_vector(normal_rng(rows_dot_product(xbar3, beta[3,s]), sigma[3]));
//}
