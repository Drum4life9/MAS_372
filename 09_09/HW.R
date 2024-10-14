library(MASS)
library(ISLR)

# a)

model = lm(mpg ~ horsepower, data=Auto)

print(summary(model))

B0 = coef(summary(model))[1]
B1 = coef(summary(model))[2]

print('Predicted MPG for Horsepower of 98: B0 + B1 * 98')
print(B0 + B1 * 98)

# b)

plot(Auto$horsepower, Auto$mpg, abline(model))

# c)
#plot(model)


# Question 14 ----------

# a)
set.seed(1)
x1=runif(100)
x2 = .05 * x1 + rnorm(100)/10
y = 2+2*x1+.03*x2+rnorm(100)

# b)
print(cor(x1,x2))
plot(x1,x2)

# c)
modelc = lm(y ~ x1 + x2)
print(summary(modelc))

# d)
modeld = lm(y ~ x1)
print(summary(modeld))

# e)
modele = lm(y ~ x2)
print(summary(modele))

# g) 
x1 = c(x1, .1)
x2 = c(x2, .8)
y  = c(y,   6)

modelc = lm(y ~ x1 + x2)
print(summary(modelc))

modeld = lm(y ~ x1)
print(summary(modeld))

modele = lm(y ~ x2)
print(summary(modele))

plot(x1,x2)


# IN CLASS
print(summary(modelc))
y_pred = 2 + 2*x1 + .3*x2
diff = y-y_pred
print(sum(diff^2))
print(1.077^2 * (100 - 2 - 1))