library('astsa')
# Question 8

# a)
x_t = varve
x_t_first = varve[1:(length(varve)/2)]
x_t_secon = varve[((length(varve)/2)+1):length(varve)]

x_var_first = var(x_t_first)
x_var_secon = var(x_t_secon)

y_t = log(x_t)
par(mfrow=c(2,1))
plot(x_t)
plot(y_t)

# c)
par(mfrow=c(1,1))
samp_acf_yt = acf(y_t)

# d)
u_t = y_t - lag(y_t, k = 1)
par(mfrow=c(2,1))
plot(u_t)
samp_acf_u_t = acf(u_t)

# f)
var_u_t = var(u_t)
p_hat_u_t_one = samp_acf_u_t$acf[2]
# Theta * sigma^2 = p_hat_u_t_one
# sigma^2 + sigma^2 + theta^2*sigma^2 = 1
# Solve systems of 2 equations
# -----------------------------------------
# theta = -2.02, sigma = +- .44
# theta = -.49, sigma = +- .90

# Question 9

# a)
St = soi
# t = 0 = 1950
data = seq(0, (37 + 2/3), 1/12)
st_data = NULL
for (i in 1:(length(St))) {
  st_data = c(st_data, St[i])
}
dat_fram = data.frame(data, st_data)
reg = lm(st_data ~ data, data=dat_fram)
print(summary(reg))

# b)
par(mfrow=c(2,1))
detrend = diff(St)
plot(detrend)
acf(detrend, lag.max = 200)
