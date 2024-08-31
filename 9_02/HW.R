df=data.frame(x=c(2,4,5,6,8), y=c(7,9,10,15,14))

summary(lm(y ~ x, data=df))

plot(df)
