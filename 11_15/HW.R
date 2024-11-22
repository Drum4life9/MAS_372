# Supp 1

set.seed(1)
# a)
print(ARMAtoMA(ar = -1/3, ma = -1/4, 4))

# b)
print(ARMAtoMA(ar = 1/4, ma = -1/3, 4))


# Question 3.8

ACF_arma_10 = ARMAacf(ar=.6, ma=0, 50)
ACF_arma_01 = ARMAacf(ar=0, ma=.9, 50)
ACF_arma_11 = ARMAacf(ar=.6, ma=.9, 50)

dev.off()
plot(ACF_arma_01, col='red')
points(ACF_arma_10, col='green', pch=12)
points(ACF_arma_11, col='blue', pch=24)


# Question 3.9

arma_10 = arima.sim(list(order=c(1,0,0), ar=.6), n = 100)
arma_01 = arima.sim(list(order=c(0,0,1), ma=.9), n = 100)
arma_11 = arima.sim(list(order=c(1,0,1), ar = .6, ma=.9), n = 100)

par(mfrow=c(2,3))
acf(arma_10, lag.max = 35)
acf(arma_01, lag.max = 35)
acf(arma_11, lag.max = 35)

acf(arma_10, lag.max = 35, type = "partial")
acf(arma_01, lag.max = 35, type = "partial")
acf(arma_11, lag.max = 35, type = "partial")
