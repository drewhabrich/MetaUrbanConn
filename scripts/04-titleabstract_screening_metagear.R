## HEADER---------------------------
## Script name: 04-title abstract screening with metagear r package
##
## Purpose of script: screen titles and abstracts of the search results
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-21
## Date last Modified: 2024-11-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list = ls())

## 1. Load relevant packages--------
pacman::p_load(tidyverse, metagear)

# 2. Read in the bibliographic data set ------------------------------------
initial_dat <- read_csv("./data/03-clean_bibliography.csv", col_names = T)

# 3. MetaGear screening ---------------------------------------------------
### Create the review excel sheet and initialize format for abstract screening
#only need to run once, uncomment if needed
# bib_unscreened <- effort_distribute(initial_dat, reviewers="drew_04", initialize = T, save_split = T, directory = "./data/") 

### Use the built in GUI for metaGear to screen articles -----
# NOTE: YOU HAVE TO SAVE BEFORE QUITTING THE GUI OR YOU'LL LOSE PROGRESS, it will update the effort_*reviewer*.csv file
abstract_screener(file = "./data/effort_drew_04.csv",
                  aReviewer = "drew_04",
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

# 4 Screening effort summary and visualization-----
#bibscreened<-effort_merge(directory="./output") #this will merge all effort_*reviewer*.csv files together
bibscreened <- read_csv("./data/effort_drew_04.csv", col_names = T)

# generate a summary table by each decision category
screensummary_table <- bibscreened %>% 
  group_by(INCLUDE) %>% #group by category
  summarize(count=n()) %>% #summarize based on count of each category
  mutate(percentage = count/nrow(bibscreened)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOtitle" ~ "Excluded based title, no indication of either urban/city or connectivity assessed",
                             INCLUDE == "NOabstr" ~ "Excluded based on abstract, either not urban/city or no metric of connectivity",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "Potential candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage))
write_csv(screensummary_table, file="./output/tables/04-initial_screenres_table.csv")

# What journals are the YES, MAYBE
bibscreened %>% 
  filter(INCLUDE=="YES" | INCLUDE=="MAYBE") %>% 
  group_by(journal) %>% 
  count(journal) %>% 
  filter(n > 1) %>% #JOURNALS WITH MORE THAN 1 ENTRY ONLY
  ggplot(aes(x=n, y = reorder(journal,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of records: YES or MAYBE") +
  theme(axis.text.y = element_text(size=5)) +  scale_y_discrete(labels = function(x) str_trunc(x, 35))

## Stacked barplot
bibscreened %>% 
  filter(INCLUDE!="NOabstr" & INCLUDE!="NOtitle") %>%
  group_by(journal, INCLUDE) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>% 
  ungroup() %>% 
  ggplot(aes(x= count, y = reorder(journal,count), fill = INCLUDE)) + #reorder to descending frequency
  geom_bar(stat="identity", position = "stack", colour="black") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme_bw()+ #to standardize to a common theme
  theme(axis.text.y = element_text(size = 5)) + #specific modifications to the theme
  scale_y_discrete(labels = function(x) str_trunc(x, 35)) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 80, 5), expand=expand_scale(mult=c(0,0.1)))

## How many entries are there for the categories
### For entries in the YES category, #What is the maximum number of articles from 1 journal?
bibscreened %>% filter(INCLUDE=="YES") %>% 
  group_by(journal) %>% count(journal) %>% 
  arrange(desc(n)) 
### For entries in the MAYBE category, #What is the maximum number of articles from 1 journal?
bibscreened %>% filter(INCLUDE=="MAYBE") %>% 
  group_by(journal) %>% count(journal) %>% 
  arrange(desc(n)) 

## Plot all the INCLUDE categories
### pull the list of journals with >5 entries
jlist <- bibscreened %>% filter(INCLUDE!="not vetted") %>% 
  group_by(journal) %>% count(journal) %>% 
  filter(n >= 5) %>% pull(journal)

bibscreened %>% 
  filter(INCLUDE!="not vetted") %>%
  filter(journal %in% jlist) %>% #un/comment this line to get ALL the journals, very cluttered with all. 
  group_by(journal, INCLUDE) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  ggplot(aes(x= count, y = reorder(journal,count), fill = INCLUDE)) + #reorder to descending frequency
    geom_bar(stat="identity", position = "stack", colour="black") +
    labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
    theme_bw()+ #to standardize to a common theme
    theme(axis.text.y = element_text(size = 5)) + #specific modifications to the theme
  scale_y_discrete(labels = function(x) str_trunc(x, 35)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 5), expand=expand_scale(mult=c(0,0.1))) 
