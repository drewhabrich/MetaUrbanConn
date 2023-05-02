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
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load relevant packages--------
library(tidyverse) #v1.3.2
library(revtools) #v0.4.1

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
clean_dat <- read_csv("./data/clean_bibliography-03-1.csv")
screen_dat <- read_bibliography("./data/final_tiab_screening-04-1.csv") #keep to class bibliography
class(screen_dat)
## get "stopwords" from commonly used terms, to avoid it suggesting these as terms
englishstopwords <- litsearchr::get_stopwords("English")
ecologystopwords <- read_lines("./raw_data/ecologystopwords.txt")
revstopwords <-revtools::revwords()
# combine into one stopword vector
stopwords<-c(englishstopwords, ecologystopwords, revstopwords)

# 3. Generate topic models to visualize concept groups ---------
## Via shiny app to interactively designate the models
topic_dat <- screen_dat %>% filter(include == "YES" | include == "MAYBE")
## Using only YES and MAYBE
td <- topic_dat
class(td)
td <- as.bibliography(topic_dat)
## 3.1 Interactive GUI method ----
screen_topics(td, remove_words = stopwords) 

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
