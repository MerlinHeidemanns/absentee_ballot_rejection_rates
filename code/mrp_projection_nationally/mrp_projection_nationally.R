








d_g <- c(0.45, 0.60)
r_g <- 1 - d_g
d <- c(0.55)
r <- 1 - d
g <- c(0.34, 0.66)

g_mail <- c(0.8, 0.4)
g_acc  <- c(0.97, 0.99)

d_after_mail <- sum(g_acc * d_g * g_mail + d_g * (1 - g_mail))