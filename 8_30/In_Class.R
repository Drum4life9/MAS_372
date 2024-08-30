library(tidyverse)

college = read.csv("../College.csv")
rownames(college)=college[,1]
college = college[,-1]
#fix(college)

college$Private[which(college$Private=="Yes")] = 1
college$Private[which(college$Private=="No")] = 0

college$Private = as.integer(college$Private)

vec = c()

for (i in 1:nrow(college)) {
  #x = c(mean(college$Undergrad), mean(college$S.F.Ratio), mean(college$Outstate))
  
  #TODO stuff here
  
  v1 = college$Undergrad - x[1]
  v2 = college$S.F.Ratio - x[2]
  v3 = college$Outstate - x[3]
  
  dists = v1^2 + v2^2 + v3^2
  
  data = data.frame(college$Private, dists)
  data = data[order(data$dists),]
  
  top = head(data, 10)
  num = sum(top$college.Private)
  
  if (num >=5) classification = "Yes"
}

