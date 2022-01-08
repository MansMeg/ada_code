## code to prepare `test_x` dataset goes here

set.seed(4711)

# X1 = shift
x1 <- c(rep(0.25, 50), rep(0.3, 50))

# X2 = treand_break
x2 <- c(seq(0.25, 0.3, length.out = 50), seq(0.3, 0.25, length.out = 50))

# X3 = dynamic
x3 <- rep(0.25, 100)
for(t in 2:length(x3)){
  x3[t] <- rnorm(1, x3[t-1], 0.005)
}

# X4 = dynamic + trend
x4 <- rep(0.25, 100)
beta <- rep(0, 100)
for(t in 2:length(x3)){
  beta[t] <- rnorm(1, beta[t-1],0.0001)
  x4[t] <- rnorm(1, x4[t-1] + beta[t], 0.005)
}

x_test <- list(jump=x1,
               trend_break=x2,
               dynamic=x3,
               dynamic_trend=x4)
usethis::use_data(x_test)
