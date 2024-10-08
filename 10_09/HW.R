library(ISLR2)
library(pls)

# Question 9
set.seed(1)

# a)
rand = sample(c(TRUE,TRUE,TRUE,TRUE,FALSE), nrow(College), replace=TRUE)
train = College[rand, ]
test = College[!rand, ]

# b)
lm = lm(Apps ~ ., data = train)

pred_lm = predict(lm, test, type = "response")
error_lm = mean((test$Apps - pred_lm)^2)

# e)
pcr_model <- pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
print(summary(pcr_model))
validationplot(pcr_model)
pred_pcr = predict(pcr_model, test, ncomp = 5)
error_pcr = mean((test$Apps - pred_pcr)^2)

# f)
pls_model <- plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")
print(summary(pls_model))
validationplot(pls_model)
pred_pls = predict(pls_model, test, ncomp = 6)
error_pls = mean((test$Apps - pred_pls)^2)
