library('ISLR')

# a)
Carseats$US = as.factor(Carseats$US)
model = lm(Sales ~ Price + Urban + US, data = Carseats)

# b)
print(summary(model))

# e)
model2 = lm(Sales ~ Price + US, data = Carseats)

# f)
print(summary(model2))
print('R^2 for model 1:')
print(summary(model)$r.squared)
print('R^2 for model 2:')
print(summary(model2)$r.squared)

# g)
print(confint(model2, level = .95))


# SUPP ----------------------------------

# a)
model3 = lm(Sales ~ Price + US + Price:US, data = Carseats)
model4 = lm(Sales ~ Price*US, data = Carseats)
print(summary(model3))
print('-----------------------')
print(summary(model4))

# b)
model5 = lm(Sales ~ Price + US + I(Price^2), data = Carseats)
model6 = lm(Sales ~ US + poly(Price, 2), data = Carseats)
print(summary(model5))
print('--------------------------------------------')
print(summary(model6))

print('------------NEW QUESTION---------------------')

# c)
model7 = lm(Sales ~ US + poly(Price, 2, raw=TRUE), data = Carseats)
print(summary(model5))
print('--------------------------------------------')
print(summary(model7))