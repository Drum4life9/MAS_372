library('MASS')


# i) 
print(cor(Boston))

model = lm(crim ~ . - crim, data = Boston)
print(summary(model))

par(mfrow=c(2,2))
plot()