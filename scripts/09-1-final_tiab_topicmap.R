## HEADER---------------------------
## Script name: revtools topicmap
##
## Purpose of script: To visualize and map the screened results and the 'grouped' topics (e.g. keyword co-occurence networks) of
## similar themes across entries. This highlights the different objectives, methods, and focus of different themes 
## in the urban landscape connectivity research corpus.
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-20
## Date last Modified: 2023-12-20
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load relevant packages--------
library(tidyverse) 
library(revtools) 
installed.packages()[names(sessionInfo()$otherPkgs), "Version"]

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
# clean_dat <- read_csv("./data/03-clean_bibliography.csv")
yes_dat <- read_bibliography("./data/08-final_tiab_screening.csv") #keep to class bibliography

yes_screened <- read_csv("./raw_data/10-full_screening/fullscreen_yes_results.csv")
## get "stopwords" from commonly used terms, to avoid it suggesting these as terms
englishstopwords <- litsearchr::get_stopwords("English")
ecologystopwords <- read_lines("./raw_data/ecologystopwords.txt")
revstopwords <-revtools::revwords()
# combine into one stopword vector
stopwords<-c(englishstopwords, ecologystopwords, revstopwords)

# 3. Generate topic models to visualize concept groups ---------
## Via shiny app to interactively designate the models
topic_dat <- yes_screened %>% filter(excl_reason == "na")
as_tibble(topic_dat)

td <- as.bibliography(topic_dat)
## 3.1 Interactive GUI method ----
screen_topics(as.data.frame(topic_dat), remove_words = stopwords) 

## 3.2 Manual method ----
dtm1 <- make_dtm(topic_dat$title, stop_words = stopwords,
         min_freq = 0.05 , max_freq = 0.85, 
         bigram_check = T, bigram_quantile = 0.90,
         retain_empty_rows = F)
tm1 <- run_topic_model(dtm1 , type="lda", n_topics = 20, 2000)
class(tm1)
screen_topics(tm1)
screen_topics(initial_dat, remove_words = stopwords)

### 4. Pickup on interactive model ----
topicmap <- readRDS("./6topicmap_screened_05.rds")
screen_topics(topicmap, remove_words = stopwords)

###
