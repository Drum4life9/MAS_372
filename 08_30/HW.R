# Q 8 ----------------------------

# a)
college = read.csv("../College.csv")
# b)
rownames(college)=college[,1]
college = college[,-1]
fix(college)

# c) 
#   i. 
print(summary(college))

#   ii.
college$Private = as.factor(college$Private)
pairs(college[,1:10])

#   iii.
boxplot(college$Outstate, college$Private, xlab="Outstate | Private")

#   iv.
Elite=rep("No", nrow(college))
Elite[college$Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)
print("--------------------")
print(summary(Elite))
boxplot(college$Outstate, college$Elite, xlab="Outstate | Elite")

#   v. 
par(mfrow=c(2,2))
hist(college$Outstate)
hist(college$PhD)
hist(college$Accept)
hist(college$Enroll)

# Q 10 -------------------------------------------------------------

library(MASS)
?Boston

# a)
# There are 506 rows and 14 columns, the rows are different suburbs in Boston
# and the columns all represent attributes about those suburbs

# d)
print("------")
print(summary(Boston$crim))
print("------")
print(summary(Boston$tax))
print("------")
print(summary(Boston$ptratio))

# The crime rates have a huge range, spanning .006 to 88.98. The highest suburbs are 381, 419, and 406
# Tax rates have a fairly large range as well, from 187 to 711. The distributions for both tax and ptRadio are fairly evenly spread
# ptRatio does not have a very large range, only 12.6 to 22

# e)

print("Suburbs by river: ")
print(sum(Boston$chas))
# 35 suburbs

# f)

print("Median ptRatio:")
print(median(Boston$ptratio))

# g)
print(min(Boston$age))
print(mean(Boston$lstat))
# Suburb 42, it has a fairly low lstat at 4.84 compared to the average of 12.65

# h)
bdf = data.frame(Boston)
print(dim(bdf[bdf$rm>7,]))
# there are 64 suburbs that have more than 7 rooms per house
print(dim(bdf[bdf$rm>8,]))
# there are 13 suburbs that have more than 8 rooms per house
print(bdf[bdf$rm>8,])
# They all roughly have an average ptratio, and the ages are all very high, except for suburb 254