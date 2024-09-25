library(ISLR)

# a) 
model <- glm(default ~ income + balance,family=binomial(link='logit'),data=Default)

errs = rep(0, 4)
valid_perc_num = .2

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
print(errs)

# d)