
# Simulation 

# Generate random numbers in normal 

library(dplyr)
library(ggplot2)
library(rgl)

x <- data.frame(rnorm(100, mean = 50, sd = 2))
y <- data.frame(rnorm(100, mean = 50, sd = 2))


xA <- function(x,y){
  x %*% y
}


s <- data.frame(rnorm(100, 12, 2))
df <- cbind(x, y)
colnames(df) <- c("x","y")
df <- df %>% 
  mutate(z = abs(x - y)) 
df %>%   
  ggplot(aes(x, y, z)) + geom_point(aes(color = z))

mycolors <- c('royalblue1', 'oldlace')
df$color <- mycolors[ as.numeric(df$z) ]

rgl::plot3d(
  x=df$x, y=df$y, z=df$z, 
  col = df$color, 
  type = 's', 
  radius = .1,
  xlab="Sepal Length", ylab="Sepal Width", zlab="Petal Length")
