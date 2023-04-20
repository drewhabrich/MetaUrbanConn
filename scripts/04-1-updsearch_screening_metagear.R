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
## Quickly verify that everything is in order:
glimpse(initial_dat)
## remove the 'database' column from the dataframe to faciliate merging results from different screening steps
dat <- initial_dat %>% select(!"database")
## initialize the dataframe for screening (uncomment below to initialize)
# bib_unscreened <- effort_distribute(dat, reviewers="drew2", initialize = T, save_split = T, directory = "./output/") #only need to run once, uncomment if needed
# colnames(bib_unscreened)

### Import the previously screened entries and merge them with the new dataframe 
### This minimizes re-screening entries multiple times
prv.screen <- read_csv("./output/effort_drew1.csv", col_names = T)
upd.screen <- read_csv("./output/effort_drew2.csv", col_names = T)

prv.screen <- prv.screen %>% select(!c("end_page","publisher","issue")) #remove useless columns

### merge based on title
merged_df <- upd.screen %>%
  left_join(prv.screen %>% select(title, INCLUDE, doi, url), by = "title", suffix = c("_upd", "_prv")) %>% #select only relevant columns
  mutate(INCLUDE = ifelse(!is.na(INCLUDE_prv), INCLUDE_prv, INCLUDE_upd)) %>% #create new INCLUDE column, if NA in previous, replace with updated 
  relocate(INCLUDE, .after = "REVIEWERS") %>% 
  select(-INCLUDE_upd, -INCLUDE_prv)

df_unscreened <- merged_df %>% filter(INCLUDE == "not vetted")
df_rev <- merged_df %>% filter(INCLUDE == "REVIEW")
df_yes <- merged_df %>% filter(INCLUDE == "YES")
df_myb <- merged_df %>% filter(INCLUDE == "MAYBE")

## Write the merged dataframe to csv for screening
write_csv(merged_df, "./output/upd_screening_effort-04-1.csv", col_names = T) #uncomment if needed, only need to do it once

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

# 4. Screening effort results ----
## read in the screened results
bibscreened <- read_csv("./output/unscreenedbib-04-1.csv", col_names = T)
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

# What journals are the YES, MAYBE, and REVIEW
bibscreened %>% 
  filter(INCLUDE=="YES" | INCLUDE=="MAYBE") %>% 
  group_by(jour_s) %>% 
  count(jour_s) %>% 
  ggplot(aes(x=n, y = reorder(jour_s,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme(axis.text.y = element_text(size=5)) +  scale_y_discrete(labels = function(x) str_trunc(x, 35))

## How many entries are there for the categories
### For entries in the YES category, #What is the maximum number of articles from 1 journal?
bibscreened %>% filter(INCLUDE=="YES") %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  arrange(desc(n)) 
### For entries in the MAYBE category, #What is the maximum number of articles from 1 journal?
bibscreened %>% filter(INCLUDE=="MAYBE") %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  arrange(desc(n)) 

### How many journals have more than 5 entries (across all decision groups)
bibscreened %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 5) %>% n_distinct() 

## Plot all the INCLUDE categories
### pull the list of journals with >5 entries
jlist <- bibscreened %>% filter(INCLUDE!="not vetted") %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  filter(n >= 5) %>% pull(jour_s)

bibscreened %>% 
  filter(INCLUDE!="not vetted") %>%
  filter(jour_s %in% jlist) %>% #un/comment this line to get ALL the journals, very cluttered with all. 
  group_by(jour_s, INCLUDE) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  ggplot(aes(x= count, y = reorder(jour_s,count), fill = INCLUDE)) + #reorder to descending frequency
  geom_bar(stat="identity", position = "stack", colour="black") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme_bw()+ #to standardize to a common theme
  theme(axis.text.y = element_text(size = 5)) + #specific modifications to the theme
  scale_y_discrete(labels = function(x) str_trunc(x, 35)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 5), expand=expand_scale(mult=c(0,0.1))) 

# 5. Full screening efforts -----------------------------------------------
updscreening <- read_csv("./output/upd_screening_effort-04-1.csv", col_names = T)
updscreening %>% filter(INCLUDE == "not vetted") %>% count("not vetted") #there should be as many rows as bibscreened df

fullscreen <- updscreening %>%
              left_join(bibscreened %>% select(STUDY_ID, INCLUDE), by = "STUDY_ID") %>% # join the two dataframes
              mutate(INCLUDE = if_else(INCLUDE.x == "not vetted", INCLUDE.y, INCLUDE.x)) %>%  #replace 'not vetted' from unscreened entries with the decisions from the upd screening
              relocate(INCLUDE, .after = "REVIEWERS") %>% 
              select(-INCLUDE.x, -INCLUDE.y)

fullscreen %>% 
  group_by(INCLUDE) %>% #group by category
  summarize(count=n()) %>% #summarize based on count of each category
  mutate(percentage = count/nrow(fullscreen)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOtitle" ~ "Excluded based title, no indication of either urban/city or connectivity assessed",
                             INCLUDE == "NOabstr" ~ "Excluded based on abstract, either not urban/city or no metric of connectivity",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "Candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage))

# TEST ZONE ----