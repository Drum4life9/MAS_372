library('MASS')


# i) 
print(cor(Boston))

pairs(Boston)

model = lm(crim ~ . - crim, data = Boston)
print(summary(model))

hist(Boston$medv)
