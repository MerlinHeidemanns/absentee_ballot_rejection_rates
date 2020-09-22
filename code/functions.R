turn_numeric <- function(df, pos){
  for (i in 1:ncol(df)){
    if (i != pos) {
      df[, i] <- as.numeric(as.character(df[,i]))
    }
  }
  return(df)
}
rnorm_truncated <- function(mu, sigma, lower, upper) {
  p_lb <- pnorm(lower, mu, sigma)
  p_ub <- pnorm(upper, mu, sigma)
  u <- runif(length(mu), p_lb, p_ub)
  y <- mu + sigma * qnorm(u)
  return(y)
}