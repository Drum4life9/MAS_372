library(ISLR2)
library(tree)

# Question 3

# Since the Gini index has pmk(1-pmk), and pm1 = 1-pm2, pm2 = 1-pm1

pm1 = seq(0, 1, 0.001)
pm2 = 1 - pm1

class_error = 1 - pmax(pm1, pm2)
gini <- pm1*(1-pm1) + pm2*(1-pm2)
entropy <- -pm1*log(pm1) - pm2*log(pm2)

data = data.frame(class_error, gini, entropy)

plot(pm1, entropy, col = "red", 
     lwd = .5, lty = 1)
lines(pm1, gini, col = "blue", lwd = 3)
lines(pm1, class_error, col = "green", lwd = 3)

legend("topright", legend=c("Entropy", "Gini", "Class"),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)


# Question 9

# a)
set.seed(1)
rand = sample(1:nrow(OJ), 800)
train = OJ[rand, ]
test = OJ[-rand, ]

# b)
tree = tree(Purchase ~ ., train)
print(summary(tree))

# c)
print('-------------------------------')
print(tree)

# d)
plot(tree)
text(tree, pretty = 0)

# e)
print('-------------------------------')
tree.pred = predict(tree, test, type = "class")
print(table(tree.pred, test$Purchase))
print("(160 + 64) / 270")
print((160 + 64) / 270)

# f)
print('-------------------------------')
cv.tree = cv.tree(tree, FUN = prune.misclass)
print(cv.tree)
min.dev = which.min(cv.tree$dev)
print("Which lowest dev?")
print(min.dev)
print("Which number of terminal nodes?")
best.size = cv.tree$size[min.dev]
print(best.size)

# g)
plot(cv.tree$size, cv.tree$dev, type = "b")

# h) 
# tree with 7 nodes!

# i)
prune.tree = prune.misclass(tree, best = best.size)

# j)
print('-------------------------------')
print(summary(prune.tree))

# k)
print('--------------Train Tree---------------')
tree.pred = predict(tree, test, type = "class")
print(table(tree.pred, test$Purchase))
print("(8 + 38) / 270")
print((8 + 38) / 270)
print('----Pruned Tree-----')
tree.pred_prune = predict(prune.tree, test, type = "class")
print(table(tree.pred_prune, test$Purchase))
print("(8 + 36) / 270")
print((8 + 36) / 270)