# 1. Load relevant packages into library ----
library(tidyverse) #v2.0.0
library(rcrossref) #v1.2.0
library(synthesisr) #v0.3.0

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
initial_dat <- read_refs(filename = "./data/deduplicated_bib-02.ris", return_df = T)

## Let's check the structure and clean the data so that we can systematically screen
glimpse(initial_dat) #16 columns, all 'character' format
## How many of the columns are *mostly* empty
initial_dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())
## Remove unnecessary columns (Keywords, notes)
initial_dat <- initial_dat %>% select(-c(keywords,notes, publisher,end_page,issue))
## Coerce year into numeric format
initial_dat <- initial_dat %>% mutate(year=as.numeric(year)) %>% as_tibble() #change year to be numeric instead of character
## Save as a seperate dataframe before any major manipulations ##
dat<-initial_dat 
glimpse(dat)

## 2.1. Tidy the dataframe and fill empty cells ----
## Currently there are empty cells in these columns:
dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% select(!c("source_type","start_page")) %>% as_tibble())

### 2.1.1. Fill Journal information
### Fill empty cells in the "journal" column with the corresponding values from the "source" column (ProQuest/SCOPUS artifact). 
dat <- dat %>% mutate(journal = if_else(is.na(journal), source, journal)) #if 'journal' is empty, fill with 'source'
### Fill the last remaining empty 'journals' with the source type if it is not a journal (e.g. report, book, or conference paper)
dat <- dat %>% mutate(journal = if_else(is.na(journal), source_type, journal),
                        author = if_else(is.na(author),"MISSING", author)) #
### Force journal names to title-case 
dat <- dat %>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(jour_s = str_to_lower(journal)) %>% #create modified column
  mutate(jour_s = str_to_title(jour_s)) %>% #coerce to title-case
  mutate(jour_s = str_replace_all(jour_s, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

### 2.1.2  Fill any missing 'year' data 
### Fill missing years based on DOI
year_doi <- dat %>% filter(is.na(year)) %>% select(doi, title, url) #filter only the entries with missing year
year_doi <- cr_works(dois = year_doi$doi)$data #extract data from crossref based on  year
glimpse(year_doi)
year_doi$year <- year(ymd(year_doi$published.online)) #classify as date and extract the year

### Join dataframes by doi and remove the redundant year and url columns
dat <- dat %>%
  left_join(year_doi %>% select(doi, year, url), by = "doi") %>% #merge by matching DOI
  mutate(year = if_else(is.na(year.x), year.y, year.x)) %>% 
  mutate(url = if_else(is.na(url.x), url.y, url.x)) %>%
  select(-year.x, -year.y, -url.x, -url.y) %>% #remove the redundant columns
  relocate(year, .after = author) %>% relocate(url, .after = doi)

# *How many empty cells are still left?* 
dat %>% summarise(across(everything(), ~ sum(is.na(.))) %>% select(!c("source_type","start_page")) %>% as_tibble())

## 2.2. Retrieve missing digital identifiers (DOI and URLs) ----
## Is there a pattern to which entries are missing DOI? Or URLS?
### 2.2.1 DOI MISSING
# what years?
dat %>% filter(is.na(doi)) %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "Search results with missing DOI by year") + xlim(1980,NA) +
  theme_bw()

# what journals?
dat %>% filter(is.na(doi)) %>% group_by(jour_s) %>% count(jour_s) %>% 
  ggplot(aes(y = reorder(jour_s, n), x=n)) + #reorder the y-axis by the n column (frequency)
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal name", title = "Search results with missing DOI by Journal") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 5)) + 
  scale_y_discrete(labels = function(x) str_trunc(x, 50))

# dat %>% filter(is.na(doi)) %>% view

### 2.2.2 URL MISSING
# what years?
dat %>% filter(is.na(url)) %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "Search results with missing URLS by year") + xlim(1980,NA)+
  theme_bw()
# large number missing for 2022
dat %>% filter(is.na(url)) %>% filter(year=="2022") %>% tibble

# what journals?
dat %>% filter(is.na(url)) %>% group_by(jour_s) %>% count(jour_s) %>% 
  ggplot(aes(y = reorder(jour_s, n), x=n)) + #reorder the y-axis by the n column (frequency)
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
                        str_c(word(author, 1, sep = ", "),"et_al", year, str_replace_all(jour_s, " ", ""),sep = "_"),#if condition is met
                        str_c(word(author, 1, sep = ", "), year, str_replace_all(jour_s, " ", ""), sep = "_" #else use this
                        )) 
  ) %>%  
  mutate(label = str_trunc(label, width = 50, side = c("right"), ellipsis = "")) %>%
  relocate(label, .before = "author")

# 3. Bibliography data exploration ----------
## check the data structure
glimpse(dat)
dat %>% summarise(across(everything(), ~ sum(is.na(.)))) #Columns with # of NAs 

# How many publications by year are there?
dat %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "'Urban landscape connectivity' search results by year") + xlim(1990,2024)+
  theme_bw()

# How many authors are there?
# what is the range of # of authors on an entry?
dat %>% ggplot(aes(x = num_authors)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "# of authors on publication", y = "Frequency") + theme_bw() + xlim(NA,30)

dat %>% summarise(mean_numauth=mean(num_authors),
                  med_numauth=median(num_authors),
                  min_numauth=min(num_authors),
                  max_numauth=max(num_authors)) #the entry with the max # of authors is a dataset publication with 199 people!

# How many journals are entries published in?
n_distinct(dat$jour_s) #distinct journal entries (this only matches on EXACT)

# Save the cleaned bibliographic dataframe to .csv for screening
clean_dat <- dat
write_csv(clean_dat, "./data/clean_bibliography-03.csv", col_names = T)

######TEST ZONE#####
# glimpse(bibs) 
# set.seed(1010)
# datm<-bibs %>% filter(INCLUDE=="YES") %>% sample_n(50, replace=F) %>% select(c("author","title","doi","journal"))
# titles <- as.vector(datm$title)
# cr_datm<-cr_works(query = titles, select = c("DOI", "title", "container-title", "published-print"), .progress = "text")