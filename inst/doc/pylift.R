## ------------------------------------------------------------------------
library(grf)
library(ggplot2)
library(uplifteval)

# Utility function round logistic function maps R -> {0,1}
rl <- function(x){
  round(1/(1+exp(-x)))
}

## ------------------------------------------------------------------------

# Generate feature data
set.seed(123)
n = 2000; p = 10
X = matrix(rnorm(n*p), n, p)
X.test = matrix(rnorm(n*p), n, p)



#
# Case 1: randomized control trial, treatment propensity is feature independent and equal
# for treatment and control cases, 50-50
#
# Treatment/Response Train/Test
set.seed(123)
W = rbinom(n, 1, 0.5)
W.test = rbinom(n, 1, 0.5)
Y = rl(rl(X[,1]) * W - rl(X[,3]) * W + rnorm(n))
Y.test = rl(rl(X.test[,1]) * W.test - rl(X.test[,3]) * W.test + rnorm(n))

tau.forest = causal_forest(X, Y, W)
tau.hat = predict(tau.forest, X.test)

plue = plUpliftEval(W.test, Y.test, tau.hat$predictions)

pl_plot(plue,
        show_practical_max = TRUE,
        show_theoretical_max = TRUE,
        show_no_dogs = TRUE,
        n_bins=20)

plue$scores

#
# Case 2: randomized control trial, treatment propensity is feature independent but unequal
# for treatment and control cases, 80-20
#
# Treatment/Response Train/Test
set.seed(123)
W = rbinom(n, 1, 0.8)
W.test = rbinom(n, 1, 0.8)
Y = rl(rl(X[,1]) * W - rl(X[,3]) * W + rnorm(n))
Y.test = rl(rl(X.test[,1]) * W.test - rl(X.test[,3]) * W.test + rnorm(n))

table(W.test, Y.test)
rowSums(table(W.test, Y.test))

tau.forest = causal_forest(X, Y, W)
tau.hat = predict(tau.forest, X.test)


pl_plot(plUpliftEval(W.test, Y.test, tau.hat$predictions),
        show_practical_max = TRUE,
        show_theoretical_max = TRUE,
        show_no_dogs = TRUE,
        n_bins=20)


