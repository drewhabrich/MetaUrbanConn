## HEADER---------------------------
## Script name: XX
##
## Purpose of script:
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-05
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load relevant packages--------
library(plyr) #plyr needs to be loaded BEFORE tidyverse to prevent issues with dplyr
library(tidyverse) #v2.0.0
library(metagear)

# 2. Import cleaned and deduplicated bibliography -----
initial_dat <- read_csv("./data/deduplicated_bib-02-2.csv", col_names = T)
# Quickly verify that everything is in order:
glimpse(initial_dat)
# initialize the dataframe for screening
#bib_unscreened <- effort_distribute(initial_dat, reviewers="drew2", initialize = T, save_split = T, directory = "./output/") #only need to run once, uncomment if needed
colnames(bib_unscreened)

### Use the built in GUI for metaGear to screen articles -----
# NOTE: YOU HAVE TO SAVE BEFORE QUITTING THE GUI OR YOU'LL LOSE PROGRESS, it will update the effort_*reviewer*.csv file
abstract_screener(file="./output/effort_drew2.csv",
                  aReviewer = "drew2",
                  unscreenedColumnName = "INCLUDE",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "abstract",
                  titleColumnName = "title",
                  browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=",
                  fontSize = 14,
                  windowWidth = 85,
                  windowHeight = 20,
                  highlightKeywords = c("urban","city","connectivity","permeability","network","isolat", "isolation",
                                        "species richness", "diversity","corridor","species", "least cost"),
                  highlightColor = "palegoldenrod",
                  theButtons = c("YES","MAYBE","NOabstr","NOtitle","REVIEW"),
                  keyBindingToButtons = c("q","w","e","r","t")) 

### 
bibscreened <- effort_merge(directory="./output")
merged_df <- merge(unique_citations, bibscreened, by = "title")
