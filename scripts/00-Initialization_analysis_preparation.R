#Use this package to communicate with Github through R
install.packages("usethis") 
library(usethis)

#Update any personal authentication token required
gitcreds::gitcreds_set() #don't need to do this every time, only when initializing on a new computer

