# Sup 3

# a)
par(mfrow=c(1,1))
set.seed(31415)
ar_mod_1 = arima.sim(list(order=c(1,0,0), ar=.9), n = 100)
plot(ar_mod_1, ylab = "x", main = (expression(AR(1)~~~phi==+.9)))
ar_res_1 = arima(ar_mod_1, order=c(1,0,0))
AR_fit_1 <- ar_mod_1 - residuals(ar_res_1)
points(AR_fit_1, type = "l", col = 2, lty = 2)


ar_mod_2 = arima.sim(list(order=c(1,0,0), ar=.4), n = 100)
plot(ar_mod_2, ylab = "x", main = (expression(AR(1)~~~phi==+.4)))
ar_res_2 = arima(ar_mod_2, order=c(1,0,0))
AR_fit_2 <- ar_mod_2 - residuals(ar_res_2)
points(AR_fit_2, type = "l", col = 2, lty = 2)

# b)
ma_mod_1 = arima.sim(list(order=c(0,0,1), ma=.9), n = 100)
plot(ma_mod_1, ylab = "x", main = (expression(MA(1)~~~delta==+.9)))
ma_res_1 = arima(ma_mod_1, order=c(0,0,1))
ma_fit_1 <- ma_mod_1 - residuals(ma_res_1)
points(ma_fit_1, type = "l", col = 2, lty = 2)

ma_mod_2 = arima.sim(list(order=c(0,0,1), ma=-.9), n = 100)
plot(ma_mod_2, ylab = "x", main = (expression(MA(1)~~~delta==-.9)))
ma_res_2 = arima(ma_mod_2, order=c(0,0,1))
ma_fit_2 <- ma_mod_2 - residuals(ma_res_2)
points(ma_fit_2, type = "l", col = 2, lty = 2)

# c)
tot_sum_ar = 0
st_dev_ar = 0
tot_sum_ma = 0
st_dev_ma = 0
for (i in 1:100) {
  ar_mod = arima.sim(list(order=c(1,0,0), ar=.9), n = 100)
  ar_res = arima(ar_mod, order=c(1,0,0))
  tot_sum_ar = tot_sum_ar + ar_res$coef[1]
  st_dev_ar = st_dev_ar + sqrt(diag(vcov(ar_res)))[1]
  
  ma_mod = arima.sim(list(order=c(0,0,1), ma=.9), n = 100)
  ma_res = arima(ma_mod, order=c(0,0,1))
  tot_sum_ma = tot_sum_ma + ma_res$coef[1]
  st_dev_ma = st_dev_ma + sqrt(diag(vcov(ma_res)))[1]
}
print('Means of coefficients')
print(tot_sum_ar / 100)
print(tot_sum_ma / 100)
print('Std dev of coefficients')
print(st_dev_ar / 100)
print(st_dev_ma / 100)