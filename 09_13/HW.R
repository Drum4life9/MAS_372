library('ISLR')

# a)
attach(Auto)
median_mpg = median(mpg)

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
train = data_random[data_random$index <= 300, ]
test = data_random[data_random$index > 300, ]