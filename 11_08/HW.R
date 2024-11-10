library('astsa')
library('broom')

# Question 1.21

# a)
set.seed(1)
w = rnorm(1600, 0, 1)
v = filter(w, sides = 2, filter=rep(1/3,3))
v = v[2:501]

samp_acf = acf(v, 20, plot=F)
print(samp_acf[20])
# Should be 0 for h > 2

# b)
set.seed(1)
w_b = rnorm(1600, 0, 1)
v_b = filter(w, sides = 2, filter=rep(1/3,3))
v_b = v[2:51]

samp_acf_b = acf(v_b, 20, plot=F)
print(samp_acf_b[20])

# Question 2.1

# a)
trend = time(jj) - 1970
Q = factor(cycle(jj))
reg = lm(log(jj)~0 + trend + Q, na.action = NULL)
summ = summary(reg)

# b)
print(summ$coefficients)

# c)
# Increase
coefs = summ$coefficients[,1]
print((coefs[5] - coefs[4]) / coefs[4])

# d) 
reg_2 = lm(log(jj)~trend + Q, na.action = NULL)
print(summary(reg_2))
# Quarter 1 gets removed, becomes less significant all around

# e)
ts.plot(jj)
preds = augment(reg)
data = data.frame(seq(-10,10.75,.25) + 1970, preds$.fitted)
plot(preds$.resid)

# Question 2.3

# a)
set.seed(1)
par(mfrow = c(2,2), mar=c(2.5,2.5,0,0) + .5, mgp = c(1.6,.6,0))
for (i in 1:4) {
  x = ts(cumsum(rnorm(100, .01, 1)))
  regx = lm(x~0 + time(x), na.action = NULL)
  plot(x, ylab='Random Walk w Drift')
  abline(a=0, b = .01, col = 2, lty = 2) # true mean, red dashes
  abline(regx, col = 4) # fitted line - blue solid
}

# b)
set.seed(1)
par(mfrow = c(2,2), mar=c(2.5,2.5,0,0) + .5, mgp = c(1.6,.6,0))
for (i in 1:4) {
  x = c(1:100 * .01)
  for (i in 1:100) {
    x[i] = x[i] + rnorm(1,0,1)
  }
  x = ts(x)
  regx = lm(x~0 + time(x), na.action = NULL)
  plot(x, ylab='Linear Trend w Noise')
  abline(a=0, b = .01, col = 2, lty = 2) # true mean, red dashes
  abline(regx, col = 4) # fitted line - blue solid
}
