data {
  int N;
  int G;
  int rejected[N, G];
  int submitted[N, G];
}
parameters {
  matrix[N, G] raw_theta;
  row_vector[G] mu;
  row_vector[G] sigma;
}
transformed parameters {
  matrix[N, G] theta;
  theta[1] = mu + raw_theta[1] .* sigma;
  for (n in 2:N) theta[n] = theta[n - 1] + raw_theta[n] .* sigma;
}
model {
  target += normal_lpdf(mu | 0, 1);
  target += normal_lpdf(sigma | 0, 1);
  target += std_normal_lpdf(to_vector(raw_theta));
  for (g in 1:G){
    target += binomial_logit_lpmf(rejected[, g] | submitted[, g], theta[, g]);
  }
}
