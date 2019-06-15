## ------------------------------------------------------------------------
library(uplifteval)

set.seed(123)

alpha <- 0.1
n <- 1000
W <- rbinom(n, 1, 0.5)
Y <- W
p1 <- Y + alpha*rnorm(n)
plot_uplift_guelman(p1, W, Y, groups=10)

## ------------------------------------------------------------------------
library(grf)

rl <- function(x){
  round(1/(1+exp(-x)))
}
n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
W = rbinom(n, 1, 0.2)
Y = rl(rl(X[,1]) * W - rl(X[,3]) * W + rnorm(n))
tau.forest = causal_forest(X, Y, W)
tau.hat = predict(tau.forest, X)
plot_uplift_guelman(tau.hat$predictions, W, Y)

## ------------------------------------------------------------------------
library(tweedie)

#
# Case: plot_uplift_guelman fails to find quantiles
#
# Generate equal prediction distributions
n <- 1000
W <- rbinom(n, 1, 0.5)
Y <- rbinom(n, 1, 0.5)
p1 <- rtweedie(n, xi=1.4, mu=1, phi=1)
hist(p1,100)

plot_uplift_guelman(p1, W, Y, groups=10)



