data {
  int J;
  int G;
  matrix[J, G] x_bar;
  matrix[J, 3] y_bar;
}
parameters {
  matrix<lower = 0, upper = 1>[G,3] beta;
  vector<lower = 0>[3] sigma;
}
model {
  for (i in 1:3) target += normal_lpdf(beta[:,1] | 0, 0.125);
  target += normal_lpdf(sigma | 0, 2);
  target += normal_lpdf(y_bar[:,1] | x_bar * to_vector(beta[:,1]), sigma[1]);
  target += normal_lpdf(y_bar[:,2] | x_bar * to_vector(beta[:,2]), sigma[2]);
  target += normal_lpdf(y_bar[:,3] | x_bar * to_vector(beta[:,3]), sigma[3]);
}
generated quantities {
  matrix[J, 3] y_bar_hat;
  for (i in 1:3){
    y_bar_hat[:,i] = to_vector(normal_rng(x_bar * to_vector(beta[:,i]), sigma[i]));
  }
}

