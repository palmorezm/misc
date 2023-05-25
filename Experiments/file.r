

# Required Setup of Rpath and Rterm
# https://stackoverflow.com/questions/65823681/setting-up-visual-studio-code-to-work-with-r-win32-cant-use-r

# Key 
# m = calories burned by metabolism only (no exercise)
# i = calories consumed in a day
# r = calories required to burn 1 pound (lb)
# d = days 
# l = pounds (lbs) 

# Settings for testing
m <- 2000
i <- 1000
r <- 3500
d <- 10
l <- 2.8571

calc.lbs <- function(m, i, r, d){
    output <- ((abs(i - m))*d)/r
    return(output)
}

calc.lbs(m,i,r,d)

calc.days <- function(m, i, r, l){
    output <- (l*r)/(abs(i-m))
    return(output)
}

calc.days(m, i, r, l)


calc.intake <- function(m, r, l, d){
    output <- -((l*r)/d)+m
    return(output)
}

calc.intake(m,r,l,d)

