
# Bionomial Probability
# Exactly x events of n trials 
# Given the probability p of population/sample

n <- 10 
x <- 3 
p <- 0.098 

part1 <- factorial(n)/(factorial(x)*factorial(n - x))
part2 <- (p^x)*(1-p)^(n-x)
product <- part1 * part2

