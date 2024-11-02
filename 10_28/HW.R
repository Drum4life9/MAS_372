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

# b)
pr = prcomp(data, scale = TRUE)
biplot(pr, scale = 0)
