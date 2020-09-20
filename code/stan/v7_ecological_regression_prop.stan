data {
  int J;
  int G;
  matrix[J, G] x_bar;
  vector[J] y_bar;
}
parameters {
  vector<lower = 0, upper = 1>[G] beta;
  real<lower = 0> sigma;
}
model {
  target += normal_lpdf(beta | 0, 0.5);
  target += normal_lpdf(sigma | 0, 2);
  target += normal_lpdf(y_bar | x_bar * beta, sigma);
}
generated quantities {
  vector[J] y_bar_hat;
  y_bar_hat = to_vector(normal_rng(x_bar * beta, sigma));
}

