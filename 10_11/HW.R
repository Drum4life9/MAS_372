library(leaps)
library(glmnet)
library(ISLR2)
# Question 8

# a)
set.seed(2)
X = rnorm(100)
e = rnorm(100)

# b)
Y = 3 + 2 * X + 1 * X^2 - 1 * X^3 + e

# e)
las_x = model.matrix(Y ~ poly(X, 10, raw = TRUE))
las_y = Y
grid = 10^seq(10, -10, length = 50)
# do training split
train = sample(1:nrow(las_x), nrow(las_x) * 4 / 5)
test = (-train)
y.test = las_y[test]


lasso = cv.glmnet(las_x[train, ], las_y[train], alpha = 1, lambda = grid)
plot(lasso)
bestlam = lasso$lambda.min
print(bestlam)
lasso.pred = predict(lasso, s = bestlam, newx = las_x[test, ], type = "coefficients")
print(lasso.pred)

# Question 9
set.seed(1)

# a)
rand = sample(c(TRUE,TRUE,TRUE,TRUE,FALSE), nrow(College), replace=TRUE)
train = College[rand, ]
test = College[!rand, ]

# c)
attach(College)
x = model.matrix(Apps ~ ., data = train)
y = train$Apps
grid = 10^seq(10, -10, length = 100)
ridge = cv.glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge)
bestlam = ridge$lambda.min
print(bestlam)
ridge.pred = predict(ridge, s = bestlam, newx = model.matrix(Apps ~ ., data = test))
test_y = test$Apps
print(test_y)
mean((ridge.pred - test_y)^2)

# d)
x = model.matrix(Apps ~ ., data = train)
y = train$Apps
grid = 10^seq(5, -1, length = 100)
lasso = cv.glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso)
bestlam = lasso$lambda.min
print(bestlam)
lasso.pred = predict(lasso, s = bestlam, newx = model.matrix(Apps ~ ., data = test))
lasso.pred_coef = predict(lasso, s = bestlam, newx = model.matrix(Apps ~ ., data = test), type = "coefficients")
test_y = test$Apps
mean((lasso.pred - test_y)^2)
print(lasso.pred_coef)

# Supp 1

# a)
set.seed(2)
variables = matrix(rnorm(100*200),nrow=200, dimnames=list(NULL, paste(1:100)))

# b)
y = rnorm(200)
data = data.frame(y, variables)

# c)
lm = lm(y ~ ., data = data)

# d)
print(summary(lm))
# 2 significant

# e) 
lm_2 = lm(y ~ X9 + X74 + X87, data = data)
print(summary(lm_2))


# Supp 2

# a)
set.seed(2)
variables = matrix(rnorm(195*200),nrow=200, dimnames=list(NULL, paste(1:195)))
y = rnorm(200)
data = data.frame(y, variables)
lm = lm(y ~ ., data = data)
print(summary(lm))

# b)
set.seed(2)
variables = matrix(rnorm(300*200),nrow=200, dimnames=list(NULL, paste(1:300)))
y = rnorm(200)
data = data.frame(y, variables)
lm = lm(y ~ ., data = data)
print(summary(lm))


