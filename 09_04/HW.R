library(MASS)
library(ISLR)

# a)

model = lm(mpg ~ horsepower, data=Auto)

print(summary(model))

B0 = coef(summary(model))[1]
B1 = coef(summary(model))[2]

print('Predicted MPG for Horsepower of 98: B0 + B1 * 98')
print(B0 + B1 * 98)

# Question 9 ---------------------------------------------------

#a)

#pairs(Auto)

# b) 

Auto["name"] <- NULL

print(cor(Auto))

# c)
model_mult = lm(mpg ~ ., data=Auto)

print(summary(model_mult))