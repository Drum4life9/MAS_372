library(astsa)
library(tseries)

# Question 23 ----------------------

# set the seed
set.seed(1)
# set up our t variable
t = 1:500

# create the xt time series
xt = 2 * cos(2*pi*(t+15)/50)
# add noise
xt_1 = xt + rnorm(500,0,1)
xt_25 = xt + rnorm(500, 0, 25)
# plot the time series
par(mfrow=c(2,1))
plot(xt_1)
plot(xt_25)

# compute the ACF to lag 100
acf_1 = acf(xt_1, lag.max = 100, plot = F)
acf_25 = acf(xt_25, lag.max = 100, plot = F)
# plot acf
plot(acf_1)
plot(acf_25)
par(mfrow=c(1,1))

# Question Sup 1 ----------------------

# a)
# Set the data to the new gtemp data set
data = gtemp_both
# Create our ARMA model from the tseries library
ARMA_2_1 = arima(data, order=c(2,0,1), method="ML")
print(ARMA_2_1)

# b)
# plot the initial data. Scaled it to support forecasting
ts.plot(data, xlim = c(1850, 2123), ylim=c(-.5, 2))

# Set up forecasting prediction by 100 years
forecast = predict(ARMA_2_1, n.ahead = 100)

# get predictions and standard error
AR_forecast <- forecast$pred
AR_forecast_se <- forecast$se

# plot the forecast predictions and standard error
points(AR_forecast, type = "l", col = 2)
points(AR_forecast - 2*AR_forecast_se, type = "l", col = 2, lty = 2)
points(AR_forecast + 2*AR_forecast_se, type = "l", col = 2, lty = 2)

# c)
# Plot the predicted values for data
resid_data = data - ARMA_2_1$residuals
points(resid_data, col = 'blue', type = 'l')

# Question Sup 2 ----------------------

# a)
# xt = 5/2 * xt_1 - 2 * xt_2 + 1/2 * xt_3 + wt

# Our phi(B) function is:
# (1 - 5/2 B + 2 B^2 - 1/2 B^3) xt = wt

# The roots of this ^ polynomial are:
# x = 1 (multiplicative of 2)
# x = 2

# The real root is inside the unit circle, meaning the
# series is not causal

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
  xt[i] = xt[i] + rnorm(1)
}

plot(xt)

# c)
# fit AR model
AR_3 = arima(xt, order=c(3,0,0))
AR_3_First_100 = arima(xt[1:100], order=c(3,0,0))

# Print models. Note first model provides an error.
print(AR_3)
print(AR_3_First_100)

# d)
yt = diff(xt)
plot(yt)

# e)
# Done on paper and submitted to document

# f)
# p for the AR model should be 2, since y_t is written in terms
# of y_t-1 and y_t-2
AR_4 = arima(yt, order=c(2,0,0))
print(AR_4)

# g)
zt = diff(yt)
plot(zt)

# h)
# Done on paper and submitted to document

# i)
# plot acf
acf(zt)
AR_zt = arima(zt, order=c(1,0,0))
print(AR_zt)

# Question Sup 3

# Read in data
arma_data <- read.csv("ARMA.csv")

# plot first 1000 observations of data
plot(arma_data$x[1:1000],type='l', main='First 1000 observations', 
     ylab="Score")

# View ACF and PACF of data, to lag 100
acf(arma_data, lag.max = 100)
pacf(arma_data, lag.max = 100)


# Grid search code
# Create results table
results <- data.frame(p = integer(), q = integer(), 
                      AIC = numeric(), stringsAsFactors = FALSE)

# Loop through p 1:6 and q 0:3
for (i in 1:6) {
  for (j in 0:3) {
    # Try to fit ARIMA model
    tryCatch(
      expr = {
        # If successful, place results in table
        mod <- arima(arma_data, order = c(i, 0, j))
        results <- rbind(results, data.frame(p = i, q = j, AIC = mod$aic))
      },
      error = function(e) {
        # If there was an error, place null AIC in table
        results <- rbind(results, data.frame(p = i, q = j, AIC = NaN))
      },
      # Diagnostic output as model is running
      finally = {
        cat("Done with: "); print(i); cat("x"); print(j)
      }
    )
    
    
  }
}

# Print raw list of results
print(results)
# Pivot the table to show a grid
wide_table <- reshape(
  results,
  idvar = "p",
  timevar = "q",
  direction = "wide"
)

# Clean column names for readability
colnames(wide_table) <- gsub("AIC.", "", colnames(wide_table))

# Print the table
print(wide_table) # We choose ARMA(2,1) 
# We opt to use ARMA(2,1)

model <- arima(arma_data, c(2,0,1))
model

# Plot ACF of residuals
acf(model$residuals, main="ACF of residuals")
# Plot the middle section of data, as well as their 
# predictions from the model fit
plot(arma_data$x[6600:6700],type='l', 
     main="Observations 6600-6700", ylab="Score")
points(arma_data$x[6600:6700] - model$residuals[6600:6700], 
       col = "blue", type = "l")

# Linear model for comparison:
time <- 1:10000
model.lm <- lm(arma_data$x ~ time)
summ <- summary(model.lm)
AIC(model.lm)
