## HEADER---------------------------
## Script name: Screening of updated search results
##
## Purpose of script: Compiling past screened and new results to be screened
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
library(metagear) #v0.7.0

# 2. Import the cleaned and deduplicated bibliography to be screened -----
initial_dat <- read_csv("./data/clean_bibliography-03-1.csv", col_names = T)
# Quickly verify that everything is in order:
glimpse(initial_dat)
# remove the 'database' column from the dataframe to faciliate merging results from different screening steps
dat <- initial_dat %>% select(!"database")
# initialize the dataframe for screening
# bib_unscreened <- effort_distribute(dat, reviewers="drew2", initialize = T, save_split = T, directory = "./output/") #only need to run once, uncomment if needed
# colnames(bib_unscreened)

### Import the previously screened entries and merge them with the new dataframe 
### This minimizes Minimize rescreening efforts
prv.screen <- read_csv("./output/effort_drew1.csv", col_names = T)
upd.screen <- read_csv("./output/effort_drew2.csv", col_names = T)

prv.screen <- prv.screen %>% select(!c("end_page","publisher","issue")) #remove useless columns

### merge based on title
merged_df <- upd.screen %>%
  left_join(prv.screen %>% select(title, INCLUDE, doi, url), by = "title", suffix = c("_upd", "_prv")) %>%
  mutate(INCLUDE = ifelse(!is.na(INCLUDE_prv), INCLUDE_prv, INCLUDE_upd)) %>% 
  relocate(INCLUDE, .after = "REVIEWERS") %>% 
  select(-INCLUDE_upd, -INCLUDE_prv)

df_unscreened <- merged_df %>% filter(INCLUDE == "not vetted")
df_rev <- merged_df %>% filter(INCLUDE == "REVIEW")
df_yes <- merged_df %>% filter(INCLUDE == "YES")
df_myb <- merged_df %>% filter(INCLUDE == "MAYBE")

## Write the merged dataframe to csv for screening
#write_csv(merged_df, "./output/upd_screening_effort-04-1.csv", col_names = T) #uncomment if needed, only need to do it once

## Write a csv for each group in the previously screened, for revision later.
# write_csv(df_unscreened, "./output/unscreenedbib-04-1.csv", col_names = T)
# write_csv(df_rev, "./output/prvscreen_review-04-1.csv", col_names = T)
# write_csv(df_yes, "./output/prvscreen_yes-04-1.csv", col_names = T)
# write_csv(df_myb, "./output/prvscreen_maybe-04-1.csv", col_names = T)

# 3. Screen updated entries - with Metagear GUI -----
# NOTE: YOU HAVE TO SAVE BEFORE QUITTING THE GUI OR YOU'LL LOSE PROGRESS, it will update the effort_*reviewer*.csv file
abstract_screener(file="./output/unscreenedbib-04-1.csv",
                  aReviewer = "drew2",
                  unscreenedColumnName = "INCLUDE",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "abstract",
                  titleColumnName = "title",
                  browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=", #query google scholar 
                  fontSize = 14,
                  windowWidth = 85,
                  windowHeight = 20,
                  highlightKeywords = c("urban","city","connectivity","permeability","network","isolat", "isolation",
                                        "species richness", "diversity","corridor", "species", "least cost", "review"),
                  highlightColor = "palegoldenrod",
                  theButtons = c("YES","MAYBE","NOabstr","NOtitle","REVIEW"),
                  keyBindingToButtons = c("q","w","e","r","t")) 

## 3.1. Rescreen prv decisions for verification
### REVIEW PILE
abstract_screener(file="./output/prvscreen_review-04-1.csv",
                  aReviewer = "drew2",
                  unscreenedColumnName = "INCLUDE",
                  unscreenedValue = "REVIEW", #RECHECKING THE 'REVIEW' CATEGORY
                  abstractColumnName = "abstract",
                  titleColumnName = "title",
                  browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=", #query google scholar 
                  fontSize = 14,
                  windowWidth = 85,
                  windowHeight = 20,
                  buttonSize = 12,
                  highlightKeywords = c("review","methodology","connectivity","framework","systematic review","theory",
                                        "theoretic", "urban", "city", "index", "indices","method"),
                  highlightColor = "skyblue",
                  theButtons = c("TopicReview","Methodology","Framework","Theoretical", "Missorted"),
                  keyBindingToButtons = c("q","w","e","r","t")) 

### YES PILE -> ONLY DO THIS IF NECESSARY
# abstract_screener(file="./output/prvscreen_yes-04-1.csv",
#                   aReviewer = "drew2",
#                   unscreenedColumnName = "INCLUDE",
#                   unscreenedValue = "YES", #RECHECKING THE 'REVIEW' CATEGORY
#                   abstractColumnName = "abstract",
#                   titleColumnName = "title",
#                   browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=", #query google scholar 
#                   fontSize = 14,
#                   windowWidth = 85,
#                   windowHeight = 20,
#                   buttonSize = 12,
#                   highlightKeywords = c("connectivity","isolation","least-cost","gene","differentiation","circuit",
#                                         "functional", "structural", "configuration", "population", "community"),
#                   highlightColor = "yellow",
#                   theButtons = c("Structural","Functional","Genetic","ObsMovement", "Reevaluate"),
#                   keyBindingToButtons = c("q","w","e","r","t")) 

# 4. WIP Screening effort results ----
## read in the screened results
glimpse(bibscreened)
bibscreened <- read_csv("./output/upd_screening_effort-04-1.csv", col_names = T)
bibscreened %>% 
  group_by(INCLUDE) %>% #group by category
  summarize(count=n()) %>% #summarize based on count of each category
  mutate(percentage = count/nrow(bibscreened)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOtitle" ~ "Excluded based title, no indication of either urban/city or connectivity assessed",
                             INCLUDE == "NOabstr" ~ "Excluded based on abstract, either not urban/city or no metric of connectivity",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "Candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage))

# TEST ZONE ----