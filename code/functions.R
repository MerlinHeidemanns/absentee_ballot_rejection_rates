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
get_int <- function(x){
  x <- as.numeric(as.character(x))
  return(x)
}
get_int_NA0 <- function(x){
  x <- as.numeric(as.character(x))
  x <- ifelse(is.na(x), 0, x)
  return(x)
}
make_num <- function(x){
  return(as.numeric(as.character(x)))
}