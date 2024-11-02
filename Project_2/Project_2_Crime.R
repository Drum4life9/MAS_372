library(ISLR2)
library(MASS)
library(leaps)
library(pls)
library(glmnet)
library(tree)

# Question 6.10 - Crime rates from different regression models

# a) 
# First, we split the Boston data set into testing and training data
# The training data contains 9/10 of the original data, which amounts
# to around 450 / 500 rows
set.seed(1)
rand = sample(c(rep(T, 9), F), nrow(Boston), replace = T)
train = Boston[rand, ]
test = Boston[!rand, ]

# ------------ Best Subset Selection ---------------
# Fit model
regfit = regsubsets(crim~., data = train, nvmax = 14)
regfit_test_mat = model.matrix(crim ~ ., data = test)
regfit_test_errors = rep(NA, 13)
for (i in 1:13) {
  coefi = coef(regfit, id = i)
  pred = regfit_test_mat[, names(coefi)] %*% coefi
  regfit_test_errors[i] = mean((test$crim - pred)^2)
}

# get Test MSE
test_MSE_regfit = regfit_test_errors[which.min(regfit_test_errors)]

# Gather summary
reg_summary = summary(regfit)
par(mfrow = c(2,2))
# Plot RSS and Adj R^2
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
# Find max Adj R^2
adj_r2_max = which.max(reg_summary$adjr2)
points(adj_r2_max, reg_summary$adjr2[adj_r2_max], col ="red", cex = 2, pch = 20)

# Plot CP and BIC
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
cp_min = which.min(reg_summary$cp) # 10
points(cp_min, reg_summary$cp[cp_min], col = "red", cex = 2, pch = 20)

plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
bic_min = which.min(reg_summary$bic) # 6
points(bic_min, reg_summary$bic[bic_min], col = "red", cex = 2, pch = 20)

# ------------ Lasso Regression ---------------
par(mfrow = c(1,1))
# Set up data structures for lasso
x_matrix = model.matrix(train$crim ~ ., data = train)
y_response = train$crim
grid = 10^seq(1.5, -1.5, length = 50)

# make lasso model, plot, and print
lasso_model = cv.glmnet(x_matrix, y_response, alpha = 1, lambda = grid)
plot(lasso_model)

# find best lambda
bestlam = lasso_model$lambda.min
print(bestlam)

# set up testing datasets
x_matrix_test = model.matrix(test$crim ~ ., data = test)
y_response_test = test$crim

lasso.pred = predict(lasso_model, s = bestlam, newx = x_matrix_test, type = "response")
test_MSE_lasso = mean((test$crim - lasso.pred)^2)

# ------------ Ridge Regression ---------------

# Set up data structures for ridge
x_matrix_ridge = model.matrix(train$crim ~ ., data = train)
y_response_ridge = train$crim
grid_ridge = 10^seq(4.5, -1.5, length = 50)

# make ridge model, plot, and print
ridge_model = cv.glmnet(x_matrix_ridge, y_response_ridge, alpha = 0, lambda = grid_ridge)
plot(ridge_model)

# find best lambda
bestlam_ridge = ridge_model$lambda.min
print(bestlam_ridge)

# set up testing datasets
x_matrix_ridge_test = model.matrix(test$crim ~ ., data = test)
y_response_ridge_test = test$crim

ridge.pred = predict(ridge_model, s = bestlam_ridge, newx = x_matrix_ridge_test, type = "response")
test_MSE_ridge = mean((test$crim - ridge.pred)^2)

# ------------ PCR Regression ---------------

# Train initial PCR model
pcr_model <- pcr(train$crim~., data = train, scale = TRUE, validation = "CV")

# Create validation plot
validationplot(pcr_model, val.type = "MSEP")

# Create prediction from test data
pcr_test_errors = rep(NA, 13)
for (i in 1:13) {
  pred = predict(pcr_model, test, ncomp = i)
  pcr_test_errors[i] = mean((test$crim - pred)^2)
}

# Create test MSE
test_MSE_pcr = pcr_test_errors[which.min(pcr_test_errors)]

# ------------ PLS Regression ---------------
pls_model = plsr(crim ~ ., data = train, validation = "CV")

# Create validation plot
validationplot(pls_model, val.type = "MSEP")

# Create prediction from test data
pls_test_errors = rep(NA, 13)
for (i in 1:13) {
  pred = predict(pls_model, test, ncomp = i)
  pls_test_errors[i] = mean((test$crim - pred)^2)
}

# Create test MSE
test_MSE_pls = pls_test_errors[which.min(pls_test_errors)]

# ------------ Tree Regression ---------------

# create tree model
tree_model = tree(crim ~ ., train)

# plot tree model
plot(tree_model)
text(tree_model, pretty = 0)

# predict using test data
tree.pred = predict(tree_model, test)

# create Tree MSE
test_MSE_tree = mean((test$crim - tree.pred)^2)

# PRUNE
cv_tree_model = cv.tree(tree_model, FUN = prune.tree)
best_tree_index = which.min(cv_tree_model$dev)
best_tree_size = cv_tree_model$size[best_tree_index]
prune_tree_model = prune.tree(tree_model, best = best_tree_size)
plot(prune_tree_model)
text(prune_tree_model)

# run prediction on pruned model
prune_tree.pred = predict(prune_tree_model, test)

# create Tree MSE
test_MSE_prune_tree = mean((test$crim - prune_tree.pred)^2)


library(ISLR2)
library(gbm)
library(leaps)
library(splines)
library(randomForest)
s_data <- Hitterss_data <- Hitterss_data <- Hitters

# part (a)
s_data <- subset(s_data, !is.na(Salary))
s_data$Salary <- log(s_data$Salary)

# part (b)
train <- s_data[1:200,]
test <- s_data[201:263,]

# part (c)
set.seed(12345)
x = c(1:10)
y = c(1:10)
test_mse <- c(1:10)
for (i in 0:10) {
  x[i] = 1/2^i
  boost.salary <- gbm(Salary ~ ., data = train, distribution = "gaussian",
                      n.trees = 1000, shrinkage = 1/2^i)
  y[i] = boost.salary$train.error[1000]
  test_mse[i] <- (mean((predict(boost.salary,test) - test$Salary)^2))  
}

plot(x,y,log='y',type='b',col="blue",xlab = "Shrinkage Parameter",
     ylab = "Training MSE")

# Part (d)
plot(x, test_mse, type='b', col="red", xlab="Shrinkage Parameter",
     ylab = "Test MSE")

# Part (e)
best_subset_model <- regsubsets(
  Salary ~ ., data = train, method = "exhaustive", nvmax = 19)

summary(best_subset_model)$cp # Model 8 is best

train$Division <- ifelse(train$Division == "E", 1, 0)
test$Division <- ifelse(test$Division == "E", 1, 0)

ns.model <- lm(Salary ~ ns(AtBat, df=4) + ns(Hits, df=4) + ns(Walks, df=4)
               + ns(Years, df=4) + ns(CRuns, df=4) + ns(CWalks, df=4) +  
                 Division + ns(PutOuts, df=4), data=train)
summary(ns.model)

ns.preds <- predict(ns.model, test)
ns.mse <- mean((ns.preds-test$Salary)^2)

# Part (g)
set.seed(12345)
bag.model <- randomForest(Salary ~ ., data=train, mtry = 19, importance = TRUE)
bag.mse <- mean((predict(bag.model, test) - test$Salary)^2)
