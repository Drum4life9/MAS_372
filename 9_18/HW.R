library(MASS)
# Question 13 ----------------

# a)
crabs = read.csv('Crabs.csv')

p_model = glm(sat ~ weight, family = poisson(), data = crabs)
print(summary(p_model))

# b)
pred = predict(p_model, data.frame(weight=2.44), type="response")
print('Predicted Y:')
print(pred)

# c)
print('------------------------------------------------')
B_hat = coef(p_model)[2]
print('B_hat:')
print(B_hat)

conf = confint(p_model, parm='weight')
print(conf)
print(10^conf[1])
print(10^conf[2])


# Question 14 ------------------------

# a)
neg_bin = glm.nb(sat ~ weight, data = crabs)
print(summary(neg_bin))

# b)
B_hat2 = coef(neg_bin)[2]
print('B_hat2:')
print(B_hat2)

conf = confint(neg_bin, parm='weight')
print(conf)