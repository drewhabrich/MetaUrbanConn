#Use this package to communicate with Github through R
install.packages("usethis") 
install.packages("tidyverse")
library(usethis) #v2.1.6
library(tidyverse) #v1.3.2

#Update any personal authentication token required
gitcreds::gitcreds_set() #don't need to do this every time, only when initializing on a new computer