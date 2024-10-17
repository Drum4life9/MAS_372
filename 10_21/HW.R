library(tree)
library(randomForest)
library(ISLR2)
library(gbm)
# Question 8

# a)
set.seed(1)
rand = sample(c(rep(T, 7), F), nrow(Carseats), replace = T)
train = Carseats[rand, ]
test = Carseats[!rand, ]

# b)
tree = tree(Sales ~ ., data = train)
plot(tree)
text(tree, pretty = 0)

pred = predict(tree, test)
test_MSE = mean((test$Sales - pred)^2)
print(test_MSE)

# c)
cv_tree_model = cv.tree(tree, FUN = prune.tree)
best_tree_index = which.min(cv_tree_model$dev)
best_tree_size = cv_tree_model$size[best_tree_index]
prune_tree_model = prune.tree(tree, best = best_tree_size)
plot(prune_tree_model)
text(prune_tree_model)

# run prediction on pruned model
prune_tree.pred = predict(prune_tree_model, test)

# create Tree MSE
test_MSE_prune_tree = mean((test$Sales - prune_tree.pred)^2)
print(test_MSE_prune_tree)

# d)
bag = randomForest(Sales ~ ., data = train, mtry = ncol(Carseats) - 1, 
                   importance = T)
yhat.bag = predict(bag, newdata = test)
plot(yhat.bag, test$Sales)
abline(0,1)
bag_MSE = mean((yhat.bag - test$Sales)^2)
print(bag_MSE)
print(importance(bag))

# e)
rand_forest = randomForest(Sales ~ ., data = train, mtry = 3, importance = T)
yhat.rand = predict(rand_forest, test)
rand_MSE = mean((test$Sales - yhat.rand)^2)
print(rand_MSE)


# Question 11

# a)
y_n = as.numeric(Caravan$Purchase) - 1
data = data.frame(Caravan, y_n)

set.seed(1)
rand = sample(1:nrow(data), 1000)
train = data[rand,]
test = data[-rand, ]

# b)
boost = gbm(y_n ~ . - Purchase, data = train, distribution = "gaussian",
            n.trees = 1000, shrinkage = .01)
print(summary(boost))

# c)
yhat.boost = predict(boost, test, type = "response")
greater_20_yhat = ifelse(yhat.boost > .2, T, F)
greater_20_actu = ifelse(test$y_n == 1, T, F)
table(greater_20_actu, greater_20_yhat)
