library(astsa)
library(forecast)
library(fGarch)
library(tseries)

# Question 3.41
data = unemp

plot(data)
plot(diff(data))
# diff data looks a lot better

acf2(data, 50)
# Acf has peaks around every year, 
# suggests something seasonal.
# The PACF has lags just after 1 of every year

model = sarima(data, 1,1,0, 0,1,0, 12)
# ok, so this has some good info
# need to bump up MA of model due to acf
model = sarima(data, 1,1,0,  0,1,1,  12)
# acf looks better! 
# still a few outliers on the normal QQ plot.
# BIC went down, but we still have some spikes
# in ACF plot. 
pacf(data, lag.max = 50)
# maybe try bumping up AR value in sarima
model = sarima(data, 2,1,0,  0,1,1,  12)
# all ACF residuals inside the cutoff, and the p values for box
# look good, suggesting no further lagging needs to be done
# for the AR component.

# Final model: SARIMA(2,1,0) x (0,1,1)_12

# For funsies
auto = auto.arima(data)
# chose Arima(5,0,0) x (2,1,1)_12
print(auto)

# Forecasting
sarima.for(data, 12, 2,1,0, 0,1,1, 12)


# Question 5.4
data = gnp
plot(data)

adf.test(data, k=0, alternative = "explosive")
adf.test(data, alternative = "explosive")
pp.test(data, alternative = "explosive")

# Question 5.6
data = oil
plot(data)
data_log = diff(log(data))
plot(data_log)
u = sarima(log(data), 2,1,1, 0,0,1, 3)
acf2(resid(u$fit)^2, 50)

gmodel = garchFit(~arma(2,1) + garch(1,0), data_log)

summary(gmodel)
