library(MASS)
library(ISLR)

# a)

model = lm(mpg ~ horsepower, data=Auto)

print(summary(model))

coeffs = summary(model)$coefficients

B0 = coeffs["(Intercept)", "Estimate"]
B1 = coeffs["horsepower", "Estimate"]

RSEB0 = coeffs["(Intercept)", "Std. Error"]
RSEB1 = coeffs["horsepower", "Std. Error"]

print('Sigma Value (RSE): -----')
print(sigma(model))

print('B0 Confidence interval: ---')
print('[B0 - 2 * RSEB0, B0 + 2 * RSEB0]')
print(B0 - 2 * RSEB0)
print(B0 + 2 * RSEB0)

print('B1 Confidence interval: ---')
print('[B1 - 2 * RSEB1, B1 + 2 * RSEB1]')
print(B1 - 2 * RSEB1)
print(B1 + 2 * RSEB1)