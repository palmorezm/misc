
# Data Visual
# Hire Rate Example
# From The_Little_Grey_Book_of_Recruiting_Benchmarks_2016.pdf


library(ggplot2)
df <- data.frame(
  matrix(c('<100', '101-10,000', '10,001+', 94, 98, 129), nrow = 3, ncol = 2))
df$X2 <- as.numeric(df$X2)
subtitle <- expression(paste("Results from ", italic("The Little Grey Book of Recruiting")))
ggplot(df, aes(reorder(X1, -X2), X2)) + 
  geom_col(aes(fill = X1, alpha = .5), color="black")  + 
  labs(title = "Candidates Per Hire by Company Size", 
       subtitle = subtitle, 
       y = "Average Number of Applicants", 
       x = "Total Number of Employees") +
  theme_minimal() + theme(legend.position = "none",
                          plot.title = element_text(hjust = 0.5), 
                          plot.subtitle = element_text(hjust = 0.5), 
                          axis.text.x = element_blank(), 
                          axis.ticks.x = element_blank(), 
                          axis.title.y = element_text(size = 12), 
                          axis.title.x = element_text(size = 12), 
                          axis.text = element_text(size = 12, angle = 0),
                          axis.ticks.y = element_blank(),
                          panel.grid = element_blank()
                          ) + 
  coord_flip() + 
  geom_text(aes(label = round(X2, 0)), size = 5, hjust = 2) 



