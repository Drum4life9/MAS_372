# Question 6
library(ISLR2)

# a)
set.seed(1)

model = glm(default ~ income + balance, 
    family = binomial(link="logit"), 
    data = Default)

print(summary(model))

# b)
boot.fn <- function(data, index) {
  coef(glm(default ~ income + balance, 
           family = binomial(link="logit"), 
           data = data,
           subset = index)) 
}

# c)
print(boot(Default, statistic = boot.fn, 100))

# Question 9

# a) 
mu_hat = mean(Boston$medv)
print(mu_hat)

# b)
std_err_mu_hat = sd(Boston$medv) / sqrt(nrow(Boston))
print(std_err_mu_hat)

# c)
boot.fn <- function(data, index) {
  sd(data[index,]$medv) / sqrt(nrow(data[index,]))
}
std_err_boot = unname(unlist(boot(Boston, statistic = boot.fn, 100)[1]))
print(std_err_boot)

# d)
print('------ T test Boston --------')
print(t.test(Boston$medv))
print('------ Bootstrap error --------')
print(mu_hat - 2 * std_err_boot)
print(mu_hat + 2 * std_err_boot)

# e)
mu_hat_med = median(Boston$medv)
print(mu_hat_med)

# f)
boot.fn <- function(data, index) {
  1.2533 * (sd(data[index,]$medv) / sqrt(nrow(data[index,])))
}
std_err_boot_med = unname(unlist(boot(Boston, statistic = boot.fn, 100)[1]))
print('------ Bootstrap error median--------')
print(mu_hat - 2 * std_err_boot_med)
print(mu_hat + 2 * std_err_boot_med)

# g)
print('------Quantiles------')
print(quantile(Boston$medv, probs = c(.1)))

boot.fn <- function(data, index) {
  quantile(Boston[index,]$medv, probs = c(.1))
}
quan_boot = boot(Boston, statistic = boot.fn, 100)
print(quan_boot)
