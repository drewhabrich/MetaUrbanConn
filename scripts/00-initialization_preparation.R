# This script is used to install and load necessary packages and set up personal authentication token for Github
### THIS SCRIPT IS NOT NECESSARY TO RUN EVERYTIME ###
#Use this package to communicate with Github through R
install.packages("usethis") 
install.packages("tidyverse")
library(usethis) 
library(tidyverse) 

#Update any personal authentication token required
gitcreds::gitcreds_get() #check if there are already credentials saved
gitcreds::gitcreds_set() #don't need to do this every time, only when initializing on a new computer