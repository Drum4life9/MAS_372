# Question 3

# a)
set.seed(1)
x1 = rnorm(1,0,1)
x2 = rnorm(1,0,1)
x_lis = c(x1, x2)

for (i in 3:100) {
  x_num = -.9 * x_lis[i - 2] + rnorm(1,0,1)
  x_lis = c(x_lis, x_num)
}
v = filter(x_lis, rep(1/4, 4), sides = 1)
plot(x_lis)
lines(v)

# b)
x_b = cos(2*pi*(1:100)/4)
v_b = filter(x_b, rep(1/4, 4), sides = 1)
plot(x_b)
lines(v_b)

# c)
x_c = cos(2*pi*(1:100)/4) + rnorm(100, 0, 1)
v_c = filter(x_c, rep(1/4, 4), sides = 1)
plot(x_c)
lines(v_c)

# Sup
x1_sup = rnorm(1,0,1)
x2_sup = rnorm(1,0,1)
x_sup = c(x1_sup, x2_sup)

for (i in 3:100) {
  x_num = -.9 * x_sup[i - 2] + rnorm(1,0,1)
  x_sup = c(x_sup, x_num)
}
v_sup = filter(x_sup * c(1, rep(0.9, length(x_sup) - 1)), 0.1, method = "recursive")
plot(x_sup)
lines(v_sup)
