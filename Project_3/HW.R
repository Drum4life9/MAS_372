# Question 23 ----------------------

# set the seed
set.seed(1)
# set up our t variable
t = 1:500

# create the xt time series
xt = 2 * cos(2*pi*(t+15)/50)
# add noise
xt = xt + rnorm(500,0,1)
# plot the time series
plot(xt)

# compute the ACF to lag 100
acf = acf(xt, lag.max = 100, plot = F)
# plot acf
plot(acf)


# Question Sup 1 ----------------------

# TODO

# Question Sup 2 ----------------------

# a)
# TODO
# Once we / I (Brian) read section 3.3
# "When a process does not depend on the future,
# such as the AR(1) when |phi| < 1, we will say
# that the process is causal

# b)
set.seed(3)

xt = NULL

for (i in 1:1000) {
  if (i == 1) {
    xt = 0
  }
  else if (i == 2) {
    xt = c(xt, 5/2 * (xt[i-1]))
  }
  else if (i == 3) {
    xt = c(xt, 5/2 * (xt[i-1]) - 2*(xt[i-2]))
  }
  else {
    xt = c(xt, 5/2 * (xt[i-1]) - 2*(xt[i-2]) + 1/2 * (xt[i-3]))
  }
  xt[i] = xt[i] + rnorm(1,0,2)
}

plot(xt)

# c)
# TODO
# After I (Brian) read about AR models (Ch 3?)

# d)
yt = diff(xt)
plot(yt)

# e)
# TODO

# f)
# TODO
# After Reading about AR

# g)
zt = diff(yt)
plot(zt)

# h)
# TODO

# i)
# TODO
# After reading about AR

# j)
# TODO