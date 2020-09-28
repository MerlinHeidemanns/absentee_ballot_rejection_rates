data {
  int N;
  int rejected[N, 2];
  int submitted[N, 2];
}
parameters {
  matrix[N, 2] raw_theta;
  row_vector[2] mu;
  row_vector[2] sigma;
}
transformed parameters {
  matrix[N, 2] theta;
  theta[1] = mu + raw_theta[1] .* sigma;
  for (n in 2:N) theta[n] = theta[n - 1] + raw_theta[n] .* sigma;
}
model {
  target += normal_lpdf(mu | 0, 1);
  target += normal_lpdf(sigma | 0, 1);
  target += std_normal_lpdf(to_vector(raw_theta));
  target += binomial_logit_lpmf(rejected[, 1] | submitted[, 1], theta[, 1]);
  target += binomial_logit_lpmf(rejected[, 2] | submitted[, 2], theta[, 2]);
}
