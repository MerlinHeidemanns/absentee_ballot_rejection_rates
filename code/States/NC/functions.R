# functions
# extract quantiles and medians
extract_quantiles_mean <- function(draws, dimensions, age_upper, 
                                   cat1_factors, cat2_factors, cat3_factors,
                                   cat_names){
  median <- apply(draws, MARGIN = dimensions, median)
  q25   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.25)))
  q75   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.75)))
  q10   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.10)))
  q90   <- apply(draws, MARGIN = dimensions, function(x) quantile(x, c(0.90)))
  len_age = dim(median)[1]
  len_cat1 = length(cat1_factors)
  len_cat2 = length(cat2_factors)
  len_cat3 = length(cat3_factors)
  if (len_cat3 != 0){
    factors = len_cat1 * len_cat2 * len_cat3
    return_df <- data.frame(
        q50 = stack(median),
        q25 = stack(q25),
        q75 = stack(q75),
        q10 = stack(q10),
        q90 = stack(q90),
        age = rep(seq(18, age_upper), factors),
        cat1 = rep(cat1_factors, rep(len_age * factors/len_cat1, len_cat1)),
        cat2 = rep(c(rep(cat2_factors, rep(len_age * factors/(len_cat1 * len_cat3), len_cat2))), len_cat1),
        cat3 = rep(c(rep(cat3_factors, rep(len_age, len_cat3))), factors/len_cat3)
    )    
  } else if (len_cat3 == 0){
    factors = len_cat1 * len_cat2
    return_df <- data.frame(
      q50 = stack(median),
      q25 = stack(q25),
      q75 = stack(q75),
      q10 = stack(q10),
      q90 = stack(q90),
      age = rep(seq(18, age_upper), factors),
      cat1 = rep(c(rep(cat1_factors, rep(len_age, len_cat1))), len_cat2),
      cat2 = rep(cat2_factors, rep(len_age * len_cat1, len_cat2))
    ) 
  }
  colnames(return_df) <- c("median", "q25", "q75", "q10", "q90", 
                           "age", cat_names)
  return(return_df)
}

create_indicator <- function(r, t){
  out <- c()
  for (i in 1:r) out <- c(out, rep(i, t))
  return(out)
}

stack <- function(mat){
  out <- c()
  for (i in 1:dim(mat)[2])  out <- c(out, mat[, i])
  return(out)
}

