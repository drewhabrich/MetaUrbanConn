## HEADER---------------------------
## Script name: updbiblio_tidying
##
## Purpose of script: Tidy up the bibliographic dataframe for screening; 
##                    filling in empty cells and and columns.
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
library(httr) #v1.4.5
library(xml2) #v1.3.3

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
### How many results were exclusively found in the first search?
updat %>% filter(database == "pastsearch") %>% count()

## check the empty rows per column
updat %>% filter(is.na(year)) %>% view #can we find by doi?
updat %>% filter(is.na(title)) %>% view #dois dont show up on crossref
updat %>% filter(is.na(journal)) %>% view #do we need these? Can I populate it from SOURCETYPE?

### 2.1.0 Check the authors column, are there any issues ----
### remove authorless entries
updat <- updat %>% filter(!is.na(author))

### find rows where the author list is messy (includes AND and ; to seperate authors)
authx <- updat %>% filter(str_detect(author, ";")) %>% 
          mutate(author = str_replace_all(author, " and ", ", ")) #replace with a semicolon
authindex <- which(str_detect(updat$author, ";")) #get the index of which rows were modified

updat$author[authindex] <- authx$author #replace according to the index

### 2.1.1. Fill Journal information ----
### Fill empty cells in the "journal" column with the corresponding values from the "source" column (ProQuest/SCOPUS artifact). 
updat <- updat %>% mutate(journal = source)
### Fill the last remaining empty 'journals' with the source type if it is not a journal (e.g. report, book, or conference paper)
updat <- updat %>% mutate(journal = if_else(is.na(journal), source_type, journal))

### Force journal names to title-case 
updat <- updat %>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(jour_s = str_to_lower(journal)) %>% #create modified column
  mutate(jour_s = str_to_title(jour_s)) %>% #coerce to title-case
  mutate(jour_s = str_replace_all(jour_s, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

### 2.1.2  Fill any missing 'year' data ----
### Fill missing years based on DOI
year_doi <- updat %>% filter(is.na(year)) %>% select(doi, title, url) #filter only the entries with missing year
year_doi <- cr_works(dois = year_doi$doi)$data #extract data from crossref based on  year
glimpse(year_doi)
year_doi$year <- year(ymd(year_doi$deposited)) #classify as date and extract the year

### Join dataframes by doi and remove the redundant year and url columns
updat <- updat %>%
  left_join(year_doi %>% select(doi, year, url), by = "doi") %>% #merge by matching DOI
  mutate(year = if_else(is.na(year.x), year.y, year.x)) %>% 
  mutate(url = if_else(is.na(url.x), url.y, url.x)) %>%
  select(-year.x, -year.y, -url.x, -url.y) %>% #remove the redundant columns
  relocate(year, .after = author) %>% relocate(url, .after = doi)

# *How many empty cells are still left?* 
updat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% select(!c("source_type","start_page")) %>% as_tibble())
## remove the two with missing years
updat <- updat %>% filter(!is.na(year))
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
updat <- get_titles(updat$doi, data = updat)

## 2.2 Create labels for each entry ----
#### count the number of authors in each row, handling both ";" and "and" as separators; NOTE: uses regex to find text strings
updat$num_authors <- sapply(strsplit(updat$author, ";\\s*|\\s+and\\s+"), length)
#### create label column in the format: Author_year_journal
updat <- updat %>%
  mutate(label = ifelse(num_authors > 2,#condition
                        str_c(word(author, 1, sep = ", "),"et_al", year, str_replace_all(jour_s, " ", ""),sep = "_"),#if condition is met
                        str_c(word(author, 1, sep = ", "), year, str_replace_all(jour_s, " ", ""), sep = "_" #else use this
                        )) 
  ) %>%  
  mutate(label = str_trunc(label, width = 50, side = c("right"), ellipsis = "")) %>%
  relocate(label, .before = "author")

# 3. Bibliography data exploration ----------
## check the data structure
glimpse(updat)
updat %>% summarise(across(everything(), ~ sum(is.na(.)))) #Columns with # of NAs 

# How many publications by year are there?
updat %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "'Urban landscape connectivity' search results by year")+
  theme(panel.background = element_rect(fill = NA),
        axis.line = element_line(linetype = "solid"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  labs(x = "Publication year")+ xlim(1980,NA)

# How many authors are there?
# what is the range of # of authors on an entry?
updat %>% ggplot(aes(x = num_authors)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "# of authors on publication", y = "Frequency") + theme_bw() + xlim(NA,30)

updat %>% summarise(mean_numauth=mean(num_authors),
                  med_numauth=median(num_authors),
                  min_numauth=min(num_authors),
                  max_numauth=max(num_authors)) #the entry with the max # of authors is a dataset publication with 199 people!

# How many journals are entries published in?
n_distinct(updat$jour_s) #distinct journal entries (this only matches on EXACT)

# Save the cleaned bibliographic dataframe to .csv for screening
clean_dat <- updat
write_csv(clean_dat, "./data/clean_bibliography-03-1.csv", col_names = T)
