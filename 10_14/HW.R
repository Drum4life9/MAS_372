library(ISLR2)
library(glmnet)
library(ggplot2)
attach(Wage)

# a)

set.seed(1)

rand = sample(c(TRUE,TRUE,TRUE,TRUE,FALSE), nrow(Wage), replace=TRUE)
train = Wage[rand, ]
test = Wage[!rand, ]

errors = c()

for (i in 1:10) {
  model_i = lm(wage ~ poly(age, i), data = train)
  pred = predict(model_i, test, type = "response")
  error_i = mean((test$wage - pred)^2)
  errors = c(errors, error_i)
}

print(which.min(errors))


model1 = lm(wage ~ poly(age, 1), data = Wage)
model2 = lm(wage ~ poly(age, 2), data = Wage)
model3 = lm(wage ~ poly(age, 3), data = Wage)
model4 = lm(wage ~ poly(age, 4), data = Wage)
model5 = lm(wage ~ poly(age, 5), data = Wage)
model6 = lm(wage ~ poly(age, 6), data = Wage)
model7 = lm(wage ~ poly(age, 7), data = Wage)
model8 = lm(wage ~ poly(age, 8), data = Wage)
model9 = lm(wage ~ poly(age, 9), data = Wage)
model10 = lm(wage ~ poly(age, 10), data = Wage)

analysis = anova(model1, model2, model3, model4, model5, 
                 model6, model7, model8, model9, model10)
print(analysis)

plot(age, wage)
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(model7, newdata = list(age = age.grid), se = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")


# b)

Wage$age.cut.2 = as.factor(cut(Wage$age, 2))
Wage$age.cut.3 = cut(Wage$age, 3)
Wage$age.cut.4 = cut(Wage$age, 4)
Wage$age.cut.5 = cut(Wage$age, 5)
Wage$age.cut.6 = cut(Wage$age, 6)
Wage$age.cut.7 = cut(Wage$age, 7)
Wage$age.cut.8 = cut(Wage$age, 8)
Wage$age.cut.9 = cut(Wage$age, 9)
Wage$age.cut.10 = cut(Wage$age, 10)

model2 = cv.glmnet(as.matrix(Wage[age.cut.2]), Wage$wage, data = Wage)
model3 = cv.glmnet(as.matrix(Wage[age.cut.3]), Wage$wage, data = Wage)
model4 = cv.glmnet(as.matrix(Wage[age.cut.4]), Wage$wage, data = Wage)
model5 = cv.glmnet(as.matrix(Wage[age.cut.5]), Wage$wage, data = Wage)
model6 = cv.glmnet(as.matrix(Wage[age.cut.6]), Wage$wage, data = Wage)
model7 = cv.glmnet(as.matrix(Wage[age.cut.7]), Wage$wage, data = Wage)
model8 = cv.glmnet(as.matrix(Wage[age.cut.8]), Wage$wage, data = Wage)
model9 = cv.glmnet(as.matrix(Wage[age.cut.9]), Wage$wage, data = Wage)
model10 = cv.glmnet(as.matrix(Wage[age.cut.10]), Wage$wage, data = Wage)

errors = c(model2$cvm[model2$index[2]], model3$cvm[model3$index[2]], model4$cvm[model4$index[2]],
           model5$cvm[model5$index[2]], model6$cvm[model6$index[2]], model7$cvm[model7$index[2]],
           model8$cvm[model8$index[2]], model9$cvm[model9$index[2]], model10$cvm[model10$index[2]])

print(which.min(errors))

plot(age, wage)
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
newData = Wage$wage[1:3000]
preds = predict(model10, Wage$wage)
lines(age.grid, preds$fit, lwd = 2, col = "blue")

# Question 9

# d)
library(splines)
attach(Boston)
dislims = range(dis)
dis.grid = seq(from = dislims[1], to = dislims[2])

fit = lm(nox ~ bs(dis, knots = c(median(Boston$dis))), data = Boston)
pred = predict(fit, newdata = list(dis = dis.grid), se = T)

print(summary(fit))

plot(Boston$dis, Boston$nox, col = "black")
lines(dis.grid, pred$fit, lwd = 2, col = "red")

# e)
fit = lm(nox ~ bs(dis, knots = c(quantile(Boston$dis))), data = Boston)
pred = predict(fit, newdata = list(dis = dis.grid), se = T)

print(summary(fit))

plot(Boston$dis, Boston$nox, col = "black")
lines(dis.grid, pred$fit, lwd = 2, col = "red")

# f)
# I'm not sure how to do this. I'm struggling enough as it is
# with this R code