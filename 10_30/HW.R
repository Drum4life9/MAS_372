library(dplyr)

# Question 8
data = USArrests

data = scale(data)

# a)
pr = prcomp(data)
pr.var = pr$sdev^2
pve = pr.var / sum(pr.var)

# b)
scores = pr$x
lis = rep(0,4)
for (i in 1:4) {
  lis[i] = sum(scores[, i]^2) / sum(data^2)
}
print(lis)

# Question 10

# a)
set.seed(1)
for (i in 1:50) {
  d_col = rep(0, 0)
  for (j in 1:3) {
    if (j == 1) {
      d_col = c(d_col, rnorm(20, -10, 6))  
    }
    else if (j == 2) {
      d_col = c(d_col, rnorm(20, 5, 3))  
    }
    else {
      d_col = c(d_col, rnorm(20, 0, 5))
    }
  }
  if (i == 1) {
    data = data.frame(d_col)
  }
  else {
    data = data.frame(data, d_col)
  }
  
}

class_no_1 = rep(1,20)
class_no_2 = rep(2,20)
class_no_3 = rep(3,20)
class_nos = c(class_no_1, class_no_2, class_no_3)
data.frame(data, class_nos)

# b)
train_data = select(data, -class_nos)
pr = prcomp(train_data, scale = TRUE)
biplot(pr, scale = 0)

# c)
km3 = kmeans(train_data, 3, nstart = 20)
print(km3$cluster)
print(km)

# d)
km2 = kmeans(train_data, 2, nstart = 20)
print(km2$cluster)

# e)
km4 = kmeans(train_data, 4, nstart = 20)
print(km4$cluster)
