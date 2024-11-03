library('astsa')

# Question 1
plot(EQ5, main='Earthquake')
lines(EXP6, col=5)


# Question Sup 1
mov_avg_1 = (20 + 18 + 16 + 20 + 50 + 18 +
  60 + 22 + 17 + 19 + 21 + (23 + 16) / 2) / 12
print(mov_avg_1)
mov_avg_2 = (60 + 20 + 18 + 16 + 20 + 50  +
               22 + 17 + 19 + 21 + 23 + (25 + 18) / 2) / 12
print(mov_avg_2)
