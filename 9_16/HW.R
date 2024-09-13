library('ISLR')

# Question 14 ----------------------------------

# a)
attach(Auto)
median_mpg = median(mpg)

mpg01 = c(nrow(Auto))

mpg01[mpg >= median_mpg] = 1 
mpg01[mpg < median_mpg] = 0

data = data.frame(subset(Auto, select = -mpg), mpg01)

# b)
attach(data)
as.factor(mpg01)
boxplot(mpg01, cylinders)
boxplot(mpg01, acceleration)
boxplot(mpg01, weight)
plot(mpg01, displacement)

# c)
data_random = data[sample(nrow(data)),]
data_random$index = 1:nrow(data_random)
train = (data_random$index <= 300)
test_data = data_random[!train, ]
test_results = data_random$mpg01[!train]

# f)
attach(data_random)

# fit the logistic model
glm.fits = glm(mpg01 ~ cylinders + displacement 
               + weight + acceleration, 
               data = data_random, family = binomial, subset = train)
glm.probs = predict(glm.fits, test_data, type = "response")
# create a list of predicted values
glm.pred = rep(0, 92)
glm.pred[glm.probs > .5] = 1
print(table(glm.pred, test_results))
print("Test error: ")
print(mean(glm.pred == test_results))

# Question AG 3.7 --------------------------------
print("------------------------")

# a)
crabs = read.csv('Crabs.csv')
# make model
model = lm(y ~ width, data = crabs)
summ = summary(model)
print(summ)
#get coeffs
coefs = coef(summ)
b0 = coefs[1]
b1 = coefs[2]
crab_filter = (crabs$weight == 5.2)
# get the specific largest weight crab
crab = crabs[crab_filter, ]
print("b0 + b1 * crab$width")
print(b0 + b1 * crab$width)

print("------------------------")
# c)
log_model = glm(y ~ weight, data = crabs, 
                family = binomial)
p = predict(log_model, data.frame(weight=5.2),  type = "response")
print("logit of log glm: ")
print(log(p/(1-p)))

print("------------------------")

pi_hat = .9968
print(log(pi_hat / (1 - pi_hat)))

# Question 3.7 ------------------

print("------------------------")

# a) 
prob_model = glm(y ~ weight, data = crabs, 
                family = binomial(link="probit"))
print(summary(prob_model))
print("------------------------")
# b)
p2 = predict(prob_model, data.frame(weight=5.2),  type = "response")
print("Pi_hat:")
print(p2)


print("------------------------")
# c)
p_low = predict(prob_model, data.frame(weight=2),  type = "response")
p_high = predict(prob_model, data.frame(weight=2.85),  type = "response")
print("Diff between quartiles")
print(p_high - p_low)