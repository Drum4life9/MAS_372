library(ISLR2)
library(MASS)
library(gam)

set.seed(1)

# Question 10

# a)
rand = sample(c(T,T,T,T,F), nrow(College), replace = T)
train = College[rand, ]
test = College[!rand, ]

empty.model <- lm(train$Outstate ~ 1, data = train)
full.model <- formula(lm(train$Outstate ~ ., data = train))
step.model <- step(empty.model, direction = "forward", scope = full.model,
                   trace = F)
print(summary(step.model))

# b)
gam1 = gam(Outstate ~ ns(Expend, 3) + ns(as.numeric(Private), 1) + 
          ns(Room.Board, 3) + ns(Terminal, 3) + ns(Grad.Rate, 3) +
          ns(perc.alumni, 3) + ns(S.F.Ratio, 3) + ns(Personal, 3) + 
          ns(Accept, 3) + ns(F.Undergrad, 3) + ns(Top10perc, 3) +
          ns(Apps, 3) + ns(Enroll, 3), data = train)
plot(gam1, se = T)

# c)
gam_MSE = mean((test$Outstate - predict(gam1, newdata = test))^2)
step_MSE = mean((test$Outstate - predict(step.model, newdata = test))^2)
print(step_MSE)
print(gam_MSE)

# d)
print(summary(gam1))
