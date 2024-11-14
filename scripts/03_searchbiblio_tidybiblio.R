## HEADER---------------------------
## Script name: 03-searchbibliography_tidybiblio
##
## Purpose of script: tidy the deduplicated bibliography results, fill empty cells and missing data
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
pacman::p_load(tidyverse, rcrossref, synthesisr)

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
initial_dat <- read_refs(filename = "./data/02-dedup_citesource.ris", return_df = T)

## Let's check the structure and clean the data so that we can systematically screen
glimpse(initial_dat) #14 columns, all 'character' format
## How many of the columns are *mostly* empty
initial_dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

## Tidy up columns and save as a seperate dataframe before any major manipulations
dat <- initial_dat %>% 
  select(-c(pubmed_id, issue, isbn, C1, start_page)) %>% #Remove unnecessary columns (Keywords, notes)
  mutate(year=as.numeric(year)) %>% # Coerce year into numeric format
  as_tibble()
  
## 2.1. Tidy the dataframe and fill empty cells ----
## Currently there are empty cells in these columns: Uncomment to check
#dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

### 2.1.1. Fill Journal information
## Fill the last remaining empty 'journals' with the source type if it is not a journal (e.g. report, book, or conference paper)
dat <- dat %>% mutate(journal = if_else(is.na(journal), "MISSING", journal),
                      author = if_else(is.na(author), "MISSING", author)) #

### 2.1.2  Fill any missing 'year' data 
### Fill missing years based on DOI
year_doi <- dat %>% filter(is.na(year)) %>% 
  select(doi, title, url) %>% 
  as_tibble(cr_works(dois = year_doi$doi)$data, .name_repair = "check_unique") %>% 
  mutate(year = year(ymd(.$published.online)))

### Join dataframes by doi and remove the redundant year and url columns
dat <- dat %>%
  left_join(year_doi %>% select(doi, year, url), by = "doi") %>% #merge by matching DOI
  mutate(year = if_else(is.na(year.x), year.y, year.x)) %>% 
  mutate(url = if_else(is.na(url.x), url.y, url.x)) %>%
  select(-year.x, -year.y, -url.x, -url.y) %>% #remove the redundant columns
  relocate(year, .after = author) %>% relocate(url, .after = doi)

## 2.2. Retrieve missing digital identifiers (DOI and URLs) ----
### 2.2.1 DOI MISSING
# what years?
dat %>% filter(is.na(doi)) %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "Search results with missing DOI by year") + xlim(1980,NA) +
  theme_bw()

# what journals?
dat %>% filter(is.na(doi)) %>% group_by(journal) %>% count(journal) %>% 
  ggplot(aes(y = reorder(journal, n), x=n)) + #reorder the y-axis by the n column (frequency)
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal name", title = "Search results with missing DOI by Journal") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5)) + 
  scale_y_discrete(labels = function(x) str_trunc(x, 50))

### 2.2.2 URL MISSING
# what years?
dat %>% filter(is.na(url)) %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "Search results with missing URLS by year") + xlim(1980,NA)+
  theme_bw()

# what journals?
dat %>% filter(is.na(url)) %>% group_by(journal) %>% count(journal) %>% 
  ggplot(aes(y = reorder(journal, n), x=n)) + #reorder the y-axis by the n column (frequency)
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal name", title = "Search results with missing URL by Journal") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5)) + 
  scale_y_discrete(labels = function(x) str_trunc(x, 50))

## 2.3 Create labels for each entry ----
#### count the number of authors in each row, handling both ";" and "and" as separators; NOTE: uses regex to find text strings
dat$num_authors <- sapply(strsplit(dat$author, ";\\s*|\\s+and\\s+"), length)
#### create label column in the format: Author_year_journal
dat <- dat %>%
  mutate(label = ifelse(num_authors > 2,#condition
                        str_c(word(author, 1, sep = ", "),"et_al", year, 
                              str_replace_all(journal, " ", ""),sep = "_"),#if condition is met
                        str_c(word(author, 1, sep = ", "), year, 
                              str_replace_all(journal, " ", ""), sep = "_" #else use this
                        )) 
  ) %>%  
  mutate(label = str_trunc(label, width = 50, side = c("right"), ellipsis = "")) %>%
  relocate(label, .before = "author")

# 3. Bibliography data exploration ----------
# How many publications by year are there?
dat %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "'Urban landscape connectivity' search results by year") + xlim(1990,2024)+
  theme_bw()

# How many journals are entries published in?
n_distinct(dat$journal) #distinct journal entries (this only matches on EXACT)

# Save the cleaned bibliographic dataframe to .csv for screening
clean_dat <- dat
write_csv(clean_dat, "./data/03-clean_bibliography.csv", col_names = T)