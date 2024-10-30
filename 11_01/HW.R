# Question 7

dat <- t(scale(t(USArrests)))
d1 <- dist(dat)^2
d2 <- as.dist(1 - cor(t(dat)))
plot(d1, d2)

# Question 9

# a)
set.seed(1)
hc <- hclust(dist(USArrests), method = "complete")

# b)
ct <- cutree(hc, 3)
sapply(1:3, function(i) names(ct)[ct == i])

# c)
hc2 <- hclust(dist(scale(USArrests)), method = "complete")

# d)
ct <- cutree(hc, 3)
sapply(1:3, function(i) names(ct)[ct == i])

# Question 13

# a)
read.csv("../Ch12Ex13.csv")

par(mfrow=c(1,3))
hc.complete <- hclust(as.dist(1 - cor(data)), method = "complete")
plot(hc.complete)

hc.complete <- hclust(as.dist(1 - cor(data)), method = "average")
plot(hc.complete)

hc.complete <- hclust(as.dist(1 - cor(data)), method = "single")
plot(hc.complete)