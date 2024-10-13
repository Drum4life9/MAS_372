library(glmnet)
library(MASS)
library(pls)

# a)
set.seed(1)

variables = matrix(rnorm(100*200),nrow=200, dimnames=list(NULL, paste(1:100)))
y = variables[, 1] - variables[, 2] + variables[, 3] - variables[, 4] + rnorm(200)

data = data.frame(y, variables)
lm = lm(y ~ ., data = data)

print(summary(lm))
# X1, 2, 3, and 4 are all highly correlated with the response (as they should be!)

# b)
set.seed(1)

variables = matrix(rnorm(195*200),nrow=200, dimnames=list(NULL, paste(1:195)))
y = variables[, 1] - variables[, 2] + variables[, 3] - variables[, 4] + rnorm(200)

data = data.frame(y, variables)
lm = lm(y ~ ., data = data)

print(summary(lm))
# The number of significant predictors has gone way down

# c)
# step.model <- regsubsets(y ~ ., data = data, nvmax = 195, method = "forward",)
# print(summary(step.model))
# adj R^2 is .8458


# f)
set.seed(1)
pls = plsr(y ~ ., data = data, validation = "CV")
print(summary(pls))
plot(pls)
