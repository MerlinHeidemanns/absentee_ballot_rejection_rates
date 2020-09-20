data {
  int J;
  int G;
  matrix[J, G] xbar1;
  matrix[J, 3] ybar;
}
transformed data {
  vector[G] ones_column = rep_vector(1, G);
  row_vector[G] ones_row = rep_vector(1, G)';
}
parameters {
  matrix<lower = 0, upper = 1>[G,3] beta;
  vector<lower = 0>[3] sigma;
}
model {
  matrix[J, G] xbar2;
  matrix[J, G] xbar3;
  xbar2 = (xbar1 .* rep_matrix(beta[:,1]', J));
  xbar2 = xbar2 ./ (xbar2 * ones_column * ones_row);
  xbar3 = (xbar2 .* rep_matrix(beta[:,2]', J));
  xbar3 = xbar3 ./ (xbar3 * ones_column * ones_row);
  for (i in 1:2) target += normal_lpdf(beta[:,i] | 0.5, 0.25);
  target += normal_lpdf(sigma | 0, 0.5);
  target += normal_lpdf(ybar[:,1] | xbar1 * beta[,1], sigma[1]);
  target += normal_lpdf(ybar[:,2] | xbar2 * beta[,2], sigma[2]);
  target += normal_lpdf(ybar[:,3] | xbar3 * beta[,3], sigma[3]);
}
generated quantities {
  //matrix[J, 3] y_bar_hat;
  //y_bar_hat[,1] = to_vector(normal_rng(xbar1 * beta[:,1], sigma[1]));
  //y_bar_hat[,2] = to_vector(normal_rng(xbar2 * beta[:,2], sigma[2]));
  //y_bar_hat[,3] = to_vector(normal_rng(xbar3 * beta[:,3], sigma[3]));
}
