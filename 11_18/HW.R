library(nlme)
library(astsa)

# Sup 1

# a)
B0 = 2
B1 = -.5

set.seed(3)

xt = NULL

for (i in 1:100) {
  if (i == 1) {
    xt[i] = B0
  }
  else {
    xt[i] = B0 + B1 * xt[i-1]
  }
  xt[i] = xt[i] + rnorm(1)
}

plot(xt)

# b)
reg = ar.ols(xt, demean = F,  intercept = T, order.max = 2)
print('--------------------------------------------------------------')
print(reg)

reg$resid[is.na(reg$resid)] = 0

# c)
acf(reg$resid, lag.max = 40)

# d)
# Sigma^2 = .7228

# e)
# The variance in Wt should just be 1, and the variance of Xt is going
# to be related by the B1 coefficient of Xt, sine we multiply B1 * Xt
# every step of the time series. We get 1 more Wt than we do B1, from 
# the very first step where t=0, so since we multiply B1 in twice, we
# can show that the variance is related by (1-B1^2) * V(Xt) = V(Wt)

# f)
ar = arima(xt, order = c(1,0,0))
print(ar)
# alpha is mu * (1 - phi1 - phi2 - ... - phip)


# Sup 2

# a)
set.seed(3)
zt = NULL
xt = NULL
for (i in 1:100) {
  if (i == 1) {
    zt[i] = 0
  }
  else {
    zt[i] = zt[i-1] * .5
  }
  zt[i] = zt[i] + rnorm(1,0,5)
  xt[i] = 70 + 2*i - 3*i^2 + zt[i]
}
plot(zt)
plot(xt)

t = (1:100)

# b)
data = data.frame(t=(1:100), xt)
fit = lm(data$xt ~ poly(data$t, 2))
print('--------------------------------------------------------------')
print(summary(fit))

# c)
acf(fit$residuals, lag.max = 50)

# d)
gls = gls(xt ~ poly(t, 2), correlation = corAR1(.5))
print('--------------------------------------------------------------')
print(summary(gls))

# f)
acf(gls$residuals, lag.max = 50)


# Sup 4

# a)
data = rec
acf(rec, lag.max = 40)
pacf(rec, lag.max = 40)

p = 2

# b)
mod_p = arima(data, order = c(p,0,0))
mod_p_min_1 = arima(data, order = c(p - 1,0,0))
mod_p_1 = arima(data, order = c(p + 1,0,0))
mod_0_2 = arima(data, order = c(0,0,2))
mod_p_2 = arima(data, order = c(p,0,2))

print('--------------------------------------------------------------')

print(mod_p$aic)
print(mod_p_min_1$aic)
print(mod_p_1$aic)
print(mod_0_2$aic)
print(mod_p_2$aic)
# yes!

# c)
pred = predict(mod_p, n.ahead = 24)

# d)
acf(mod_p$residuals)

# e)
ts.plot(rec, pred$pred, col = 1:2, xlim=c(1980,1990))
U = pred$pred + pred$se 
L = pred$pred - pred$se
xx = c(time(U), rev(time(U)))
yy = c(L, rev(U))
polygon(xx,yy, border = 8, col = gray(.6, alpha = .2))
lines(pred$pred, type = "p", col = 2)

# f)
pred = predict(mod_0_2, n.ahead = 24)
acf(mod_0_2$residuals)
ts.plot(rec, pred$pred, col = 1:2, xlim=c(1980,1990))
U = pred$pred + pred$se 
L = pred$pred - pred$se
xx = c(time(U), rev(time(U)))
yy = c(L, rev(U))
polygon(xx,yy, border = 8, col = gray(.6, alpha = .2))
lines(pred$pred, type = "p", col = 2)
