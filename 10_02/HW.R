library(leaps)
# Question 8

# a)
set.seed(2)
X = rnorm(100)
e = rnorm(100)

# b)
Y = 3 + 2 * X + 1 * X^2 - 1 * X^3 + e

# c)
data = data.frame(Y, X)

regfit.full <- regsubsets(Y ~ X, data, nvmax=10, method = "forward")

plot(X, Y)
