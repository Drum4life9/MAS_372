library(ISLR)

#Question 5
# a) 
model <- glm(default ~ income + balance,
             family=binomial(link='logit'),
             data=Default)

errs = rep(0, 4)
valid_perc_num = .3

for (i in 1:4) {
  set.seed(i)
  
  # b)
  sample <- sample(c(TRUE,FALSE), nrow(Default),  
                   replace=TRUE, prob=c((1 - valid_perc_num),valid_perc_num))
  
  traini <- Default[sample, ]
  validi<- Default[!sample, ]
  
  glmi = glm(default ~ income + balance,
             family=binomial(link='logit'),
             data=traini)
  vali <- ifelse(predict(glmi, validi, type="response") >= .5, "Yes", "No")
  sumi <- sum(ifelse(validi['default'] == vali, 1, 0))
  erri <- 1 - (sumi / (nrow(validi)))
  
  errs[i] <- erri
}

# c)
print('Errors: ')
print(errs)

# d)
stud_dummy_yes = ifelse(Default$student == "Yes", 1, 0)
Default$stud_dummy_yes = stud_dummy_yes
#------ do stuff -----
errs_dum= rep(0, 4)
for (i in 1:4) {
  set.seed(i+5)
  sample <- sample(c(TRUE,FALSE), nrow(Default),  
                   replace=TRUE, prob=c((1 - valid_perc_num),valid_perc_num))
  
  train_dum <- Default[sample, ]
  valid_dum<- Default[!sample, ]
  
  glm_dum = glm(default ~ income + balance + stud_dummy_yes,
             family=binomial(link='logit'),
             data=train_dum)
  val_dum <- ifelse(predict(glm_dum, valid_dum, type="response") >= .5, "Yes", "No")
  sum_dum <- sum(ifelse(valid_dum['default'] == val_dum, 1, 0))
  err_dum <- 1 - (sum_dum / (nrow(valid_dum)))
  errs_dum[i] <- err_dum
}
print('Dummy errors:')
print(errs_dum)


# Question 7

# a)
glm = glm(Direction ~ Lag1 + Lag2, 
          family = binomial(link = "logit"), 
          data = Weekly)
# b)

glm_min_1 = glm(Direction ~ Lag1 + Lag2, 
                family = binomial(link = "logit"), 
                data = Weekly[-1, ])

# c)
classify = predict(glm, Weekly[1, ], type = "response")

# d)
numCorrect = 0

for (i in 1:nrow(Weekly)) {
  glm_min_i = glm(Direction ~ Lag1 + Lag2, 
                  family = binomial(link = "logit"), 
                  data = Weekly[-i, ])
  classify = predict(glm, Weekly[i, ], type = "response")
  pred = ifelse(classify >= .5, "Up", "Down")
  correct = (pred == Weekly[i, ]["Direction"])
  if (correct) {
    numCorrect = numCorrect + 1
  }
}

avg = numCorrect * 1.0 / nrow(Weekly)
print("Average number correct: ")
print(avg)
