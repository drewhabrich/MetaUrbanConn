## HEADER---------------------------
## Script name: updbiblio_tidying
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

# 1. Load relevant packages into library ----
library(tidyverse) #v2.0.0
library(rcrossref) #v1.2.0
library(synthesisr) #v0.3.0

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
upd_dat <- read_refs(filename = "./data/deduplicated_bib-02-2.ris", return_df = T)

# Take a peek at the dataframes
glimpse(upd_dat)

## Remove unnecessary columns (Keywords, notes)
upd_dat <- upd_dat %>% select(-c(pubmed_id, issue, isbn, notes, C1))
## Coerce year into numeric format
upd_dat <- upd_dat %>% mutate(year=as.numeric(year)) %>% as_tibble() #change year to be numeric instead of character
## Save as a seperate dataframe before any major manipulations ##
updat <- upd_dat

## 2.1. Tidy the dataframe and fill empty cells ----
## Currently there are empty cells in these columns:
updat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% select(!c("source_type","start_page","database")) %>% as_tibble())

### remove the entries in past_search
updat %>% filter(is.na(author) | is.na(year)) %>% view
updat <- 
### 2.1.1. Fill Journal information
### Fill empty cells in the "journal" column with the corresponding values from the "source" column (ProQuest/SCOPUS artifact). 
updat <- updat %>% mutate(journal = source)
### Fill the last remaining empty 'journals' with the source type if it is not a journal (e.g. report, book, or conference paper)
dat <- dat %>% mutate(journal = if_else(is.na(journal), source_type, journal),
                      author = if_else(is.na(author),"MISSING", author)) #
### Force journal names to title-case 
dat <- dat %>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(jour_s = str_to_lower(journal)) %>% #create modified column
  mutate(jour_s = str_to_title(jour_s)) %>% #coerce to title-case
  mutate(jour_s = str_replace_all(jour_s, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase