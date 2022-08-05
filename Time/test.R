

install.packages("remotes")
# Install using the remotes package
remotes::install_github("rstudio/shinyuieditor")

library(shinyuieditor)
?shinyuieditor::launch_editor()
shinyuieditor::launch_editor(app_loc = "app.R")
shinyuieditor::launch_editor(app_loc = "misc/Time/ui.R")
