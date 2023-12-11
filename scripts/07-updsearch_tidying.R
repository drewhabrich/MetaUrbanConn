## HEADER---------------------------
## Script name: updbiblio_tidying
##
## Purpose of script: Tidy up the bibliographic dataframe for screening; 
##                    filling in empty cells and and columns.
## Author: Andrew Habrich
##
## Date Created: 2023-04-05
## Date last Modified: 2023-10-26 
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
library(httr) #v1.4.5
library(xml2) #v1.3.3

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
updated_searchdat <- read_refs(filename = "./data/06-dedup_updsearch.ris", return_df = T)

# Take a peek at the dataframes
glimpse(updated_searchdat)

## initial cleaning
upd_dat <- updated_searchdat %>% select(-c(pubmed_id, issue, isbn, C1)) %>% ## Remove unnecessary columns (Keywords, notes)
  mutate(year = as.numeric(year)) %>% ## Coerce year into numeric format
  as_tibble()

## 2.1. Tidy the dataframe and fill empty cells ----
## Currently there are empty cells in these columns:
upd_dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

### How many results were detected in the past search?
upd_dat %>% filter(str_detect(source, "pastsearch")) %>% count()
### How many results were exclusively found in the first search?
upd_dat %>% filter(grepl("^pastsearch$", source)) %>% count()


### 2.1.0 Check the authors column, are there any issues ----
### remove authorless entries
upd_dat %>% filter(is.na(author))

### find rows where the author list is messy (includes AND and ; to seperate authors)
authx <- upd_dat %>% filter(str_detect(author, ";")) %>% 
          mutate(author = str_replace_all(author, " and ", ", ")) #replace with a semicolon
authindex <- which(str_detect(upd_dat$author, ";")) #get the index of which rows were modified

upd_dat$author[authindex] <- authx$author #replace according to the index

# ### 2.1.1. Fill Journal information
# ### Force journal names to title-case 
# updat <- updat %>% 
#   mutate(journal = str_replace_all(journal, "&", "and")) %>% 
#   mutate(jour_s = str_to_lower(journal)) %>% #create modified column
#   mutate(jour_s = str_to_title(jour_s)) %>% #coerce to title-case
#   mutate(jour_s = str_replace_all(jour_s, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

### 2.1.2  Fill any missing 'year' data ----
### Fill missing years based on DOI
year_doi <- upd_dat %>% filter(is.na(year)) %>% select(doi, title, url) #filter only the entries with missing year
year_doi <- cr_works(dois = year_doi$doi)$data #extract data from crossref based on  year
year_doi$year <- year(ymd(year_doi$deposited)) #classify as date and extract the year

### Join dataframes by doi and remove the redundant year and url columns
upd_dat <- upd_dat %>%
  left_join(year_doi %>% select(doi, year, url), by = "doi") %>% #merge by matching DOI
  mutate(year = if_else(is.na(year.x), year.y, year.x)) %>% 
  mutate(url = if_else(is.na(url.x), url.y, url.x)) %>%
  select(-year.x, -year.y, -url.x, -url.y) %>% #remove the redundant columns
  relocate(year, .after = author) %>% relocate(url, .after = doi)

# *How many empty cells are still left?* 
upd_dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

## find the missing titles from doi.org
### Define function to retrieve titles for a list of DOIs
get_titles <- function(dois, data) {
  for (i in seq_along(dois)) {
    if (is.na(data$title[i])) {  # check if title is missing
      doi <- dois[i]
      url <- paste0("https://doi.org/", doi)
      response <- GET(url)
      if (status_code(response) == 200) {
        html <- read_html(rawToChar(response$content))
        title <- xml_text(xml_find_first(html, "//title"))
        title <- sub(" \\|.*", "", title)  # remove any text after "|"
        data$title[i] <- title
      } else {
        message(paste0("Error retrieving title for DOI ", doi))
      }
    }
  }
  return(data)
}
upd_dat <- get_titles(upd_dat$doi, data = upd_dat)

## 2.2 Create labels for each entry ----
#### count the number of authors in each row, handling both ";" and "and" as separators; NOTE: uses regex to find text strings
upd_dat$num_authors <- sapply(strsplit(upd_dat$author, ";\\s*|\\s+and\\s+"), length)
#### create label column in the format: Author_year_journal
upd_dat <- upd_dat %>%
  mutate(label = ifelse(num_authors > 2,#condition
                        str_c(word(author, 1, sep = ", "),"et_al", year, str_replace_all(journal, " ", ""),sep = "_"),#if condition is met
                        str_c(word(author, 1, sep = ", "), year, str_replace_all(journal, " ", ""), sep = "_" #else use this
                        )) 
  ) %>%  
  mutate(label = str_trunc(label, width = 50, side = c("right"), ellipsis = "")) %>%
  relocate(label, .before = "author")

# 3. Bibliography data exploration ----------
## check the data structure
glimpse(upd_dat)
upd_dat %>% summarise(across(everything(), ~ sum(is.na(.)))) #Columns with # of NAs 

# How many publications by year are there?
upd_dat %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "'Urban landscape connectivity' search results by year")+
  theme(panel.background = element_rect(fill = NA),
        axis.line = element_line(linetype = "solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(x = "Publication year")+ xlim(1980,NA)

# How many authors are there?
# what is the range of # of authors on an entry?
upd_dat %>% group_by(num_authors) %>% count()

upd_dat %>% ggplot(aes(x = num_authors)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "# of authors on publication", y = "Frequency") + theme_bw() + xlim(NA,30)

upd_dat %>% summarise(mean_numauth=mean(num_authors),
                  med_numauth=median(num_authors),
                  min_numauth=min(num_authors),
                  max_numauth=max(num_authors)) #the entry with the max # of authors is a dataset publication with 199 people!

# Save the cleaned bibliographic dataframe to .csv for screening
clean_dat <- upd_dat
write_csv(clean_dat, "./data/07-clean_upd_bibliography.csv", col_names = T)
