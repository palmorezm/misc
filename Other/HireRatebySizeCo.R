
# Data Visual
# Hire Rate Example
# From The_Little_Grey_Book_of_Recruiting_Benchmarks_2016.pdf


library(ggplot2)
df <- data.frame(
  matrix(c('<100', '101-10,000', '10,001+', 94, 98, 129), nrow = 3, ncol = 2))
df$X2 <- as.numeric(df$X2)
ggplot(df, aes(X1, X2)) + geom_col(aes(fill=X2))
