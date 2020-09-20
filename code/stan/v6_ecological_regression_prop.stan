data {
  int J;
  int S;
  int s[J];
  int G;
  matrix[J, G] x_bar;
  matrix[J, 3] y_bar;
}
parameters {
  matrix<lower = 0, upper = 1>[G,3] beta_mean;
  matrix<lower = 0>[G, 3] beta_sigma; 
  matrix<lower = 0, upper = 1>[3,S] beta[G];
  vector<lower = 0>[3] sigma;
}
model {
  for (i in 1:3) target += normal_lpdf(beta_mean[:,1] | 0, 0.5);
  for (i in 1:3) target += normal_lpdf(beta_sigma[:,1] | 0, 0.5);
  for (g in 1:G) for (i in 1:3) target += normal_lpdf(beta[g,i,:] | beta_mean[g, i], beta_sigma[g, i]);
  target += normal_lpdf(sigma | 0, 2);
  target += normal_lpdf(y_bar[:,1] | x_bar * beta[:,1, s], sigma[1]);
  target += normal_lpdf(y_bar[:,2] | rep_vector(to_vector(beta[:,1, s])' * to_vector(beta[:,2, s]), J), sigma[2]);
  target += normal_lpdf(y_bar[:,3] | rep_vector(to_vector(beta[:,2, s])' * to_vector(beta[:,3, s]), J), sigma[3]);
}
generated quantities {
  matrix[J, 3] y_bar_hat;
  for (i in 1:3){
    y_bar_hat[:,i] = to_vector(normal_rng(x_bar * to_vector(beta[:,i]), sigma[i]));
  }
}

