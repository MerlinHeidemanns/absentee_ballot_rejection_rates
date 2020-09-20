data {
  int J;
  int G;
  matrix[J, G] x_bar1;
  matrix[J, 3] y_bar;
}
parameters {
  matrix<lower = 0, upper = 1>[G,3] beta;
  matrix[J, G] beta2_hat;
  matrix[J, G] beta3_hat;
  vector<lower = 0>[3] sigma;
}
transformed parameters {
  matrix [J, G] x_bar2;
  matrix [J, G] x_bar3;
  for (j in 1:J){
    x_bar2[j]  = softmax(x_bar1[j]' .* beta[:,1])';
    x_bar3[j]  = softmax(x_bar2[j]' .* beta[:,2])';
  }
}

model {
  for (i in 1:3) target += normal_lpdf(beta[:,1] | 0, 0.5);
  target += normal_lpdf(sigma | 0, 2);
  target += normal_lpdf(y_bar[:,1] | x_bar1 * beta[:,1], sigma[1]);
  target += normal_lpdf(y_bar[:,2] | x_bar2 * beta[:,2], sigma[2]);
  target += normal_lpdf(y_bar[:,3] | x_bar3 * beta[:,3], sigma[3]);
}
generated quantities {
  matrix[J, 3] y_bar_hat;
  y_bar_hat[:,1] = to_vector(normal_rng(x_bar1 * beta[:,1], sigma[1]));
  y_bar_hat[:,2] = to_vector(normal_rng(x_bar2 * beta[:,2], sigma[2]));
  y_bar_hat[:,3] = to_vector(normal_rng(x_bar3 * beta[:,3], sigma[3]));
}

