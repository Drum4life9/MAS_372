library(astsa)
library(stats)
library(forecast)
library(tseries)

# Question 3.30

data = log(varve[1:100])
h1 = HoltWinters(data, alpha=.75, beta = F, gamma = F)
h2 = HoltWinters(data, alpha=.5, beta = F, gamma = F)
h3 = HoltWinters(data, alpha=.25, beta = F, gamma = F)

par(mfrow=c(3,1))
plot(h1)
plot(h2)
plot(h3)

print('------------------------')
print('h1:')
print(h1$coefficients)
print('h2:')
print(h2$coefficients)
print('h3:')
print(h3$coefficients)


# Question 3.32
data = oil
par(mfrow=c(1,1))
# Always plot the data
plot(data)

# Maybe look at differencing?
plot(diff(data))
# Eh, not so great, but maybe

data = diff(data)

# plot ACF and PACF
par(mfrow=c(2,1))
acf(data, lag.max = 50)
pacf(data, lag.max = 20)
# ACF cuts off after 2, PACF seems to tail. Suggests something with ARIMA(?,1,2).

model = arima(data, order=c(0,1,2))

print(model)

acf(model$residuals)
pacf(model$residuals)

# Strange cutoffs in PACF. Let's try auto.arima, turning seasonal off
model_auto = auto.arima(data, seasonal = F)

# Auto arima suggests a ARIMA(1,1,2) model with seasonality off. 

checkresiduals(model)
checkresiduals(model_auto)
# let's use the 1,1,2 model from auto arima


# Question 3.39
phi = c(rep(0,11),.8)
ACF = ARMAacf(ar=phi, ma=.5, 50)[-1] 
par(mfrow=c(1,1))
plot(ACF, type="h", xlab="LAG", ylim=c(-.4,.8)); abline(h=0)
