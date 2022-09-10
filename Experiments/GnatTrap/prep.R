
# Gnat Trap
# Preparation Script

df <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSC4Y0ZRfzQLbwILHYwOJz3srSZRf3m-3V3srZ5JDfx3giq-rzE9qswEy-upWXmUa5o7poth9IENXYg/pub?output=csv")

library(lubridate)
df <- df[lubridate::mdy(df$Date) >= "2022-07-14",]
df$GnatCount <- as.numeric(df$GnatCount)
df$CupID <- as.factor(df$CupID)
df$Date <- lubridate::mdy(df$Date)
df$Time <- parse_time(df$Time)
df$DateTime <- parse_datetime(paste(df$Date, df$Time))

int1 <- interval(start = min(df$DateTime), end = max(df$DateTime))
plot(int1, y = c(1:1))
plot(df$DateTime, rep(10, length(df$DateTime)))
plot(df$DateTime, df$GnatCount)

library(ggplot2)
df %>% 
  ggplot(., aes(DateTime, GnatCount, fill = Type)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "black")



library(tidymodels)  # for the parsnip package, along with the rest of tidymodels

# Helper packages
library(readr)       # for importing data
library(broom.mixed) # for converting bayesian models to tidy tibbles
library(dotwhisker)  # for visualizing regression results


urchins <-
  # Data were assembled for a tutorial 
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>% 
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

ggplot(urchins,
       aes(x = initial_volume, 
           y = width, 
           group = food_regime, 
           col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)
#> `geom_smooth()` using formula 'y ~ x'

linear_reg() %>% 
  set_engine("keras")

lm_mod <- linear_reg()

lm_fit <- 
  lm_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)
lm_fit
tidy(lm_fit)
tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))
new_points <- expand.grid(initial_volume = 20, 
                          food_regime = c("Initial", "Low", "High"))
mean_pred <- predict(lm_fit, new_data = new_points)
conf_int_pred <- predict(lm_fit, 
                         new_data = new_points, 
                         type = "conf_int")
# Now combine: 
plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# and plot:
ggplot(plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2) + 
  labs(y = "urchin size")



# set the prior distribution
prior_dist <- rstanarm::student_t(df = 1)

set.seed(123)

# make the parsnip model
bayes_mod <-   
  linear_reg() %>% 
  set_engine("stan", 
             prior_intercept = prior_dist, 
             prior = prior_dist) 

# train the model
bayes_fit <- 
  bayes_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

print(bayes_fit, digits = 5)
#> parsnip model object
#> 
#> stan_glm
#>  family:       gaussian [identity]
#>  formula:      width ~ initial_volume * food_regime
#>  observations: 72
#>  predictors:   6
#> ------
#>                                Median   MAD_SD  
#> (Intercept)                     0.03373  0.00963
#> initial_volume                  0.00153  0.00040
#> food_regimeLow                  0.01916  0.01290
#> food_regimeHigh                 0.02090  0.01477
#> initial_volume:food_regimeLow  -0.00124  0.00051
#> initial_volume:food_regimeHigh  0.00053  0.00072
#> 
#> Auxiliary parameter(s):
#>       Median  MAD_SD 
#> sigma 0.02130 0.00184
#> 
#> ------
#> * For help interpreting the printed output see ?print.stanreg
#> * For info on the priors used see ?prior_summary.stanreg

tidy(bayes_fit, conf.int = TRUE)
bayes_plot_data <- 
  new_points %>% 
  bind_cols(predict(bayes_fit, new_data = new_points)) %>% 
  bind_cols(predict(bayes_fit, new_data = new_points, type = "conf_int"))

ggplot(bayes_plot_data, aes(x = food_regime)) + 
  geom_point(aes(y = .pred)) + 
  geom_errorbar(aes(ymin = .pred_lower, ymax = .pred_upper), width = .2) + 
  labs(y = "urchin size") + 
  ggtitle("Bayesian model with t(1) prior distribution")


urchins %>% 
  group_by(food_regime) %>% 
  summarize(med_vol = median(initial_volume))

bayes_mod %>% 
  fit(width ~ initial_volume * food_regime, data = urchins)

pred(bayes_mod)
predict(bayes_mod, bayes_plot_data)
?predict()

# Example of Cars Prediction Pre-Tidymodels
df <- datasets::cars

#Creates a linear model
my_linear_model <- lm(dist~speed,data = df)

#Prints the model results 
my_linear_model

#Creating a data frame
variable_speed <- data.frame(speed=c(11,11,12,12,12,12,13,13,13,13))

#fiting the linear model
liner_model <- lm(dist~speed,data = df)

#predicts the future values
predict(liner_model,newdata = variable_speed)


#Input data
variable_speed <-data.frame(speed=c(11,11,12,12,12,12,13,13,13,13))

#Fits the model
liner_model <- lm(dist~speed,data = df)

#Predits the values with confidence interval 
predict(liner_model,newdata = variable_speed,interval = 'confidence')
