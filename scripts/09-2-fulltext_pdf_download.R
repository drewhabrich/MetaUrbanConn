## HEADER---------------------------
## Script name: fulltext pdf download
##
## Purpose of script: Download the articles screened as maybe or yes, for full-text
## screening to determine eligibility; not all articles are available open-access so 
## probably going to have to come up with something to download in bulk, to minimize manual downloading.
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-26
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load packages ------------------------------------------------
library(tidyverse) #v2.0.0
library(rcrossref) #v1.2.0
## packages still in-development 
#library(roadoi) #v0.7.2 Find Free Versions of Scholarly Publications via Unpaywall
#library(heapsofpapers) #v0.1.0 download pdfs from URL

# 2. Import and peek at the data ---------------------------------------
final_tiab <- read_csv("./data/08-final_tiab_screening.csv")
## Quick glimpse of the dataframe structure, and reorganize anything if needed
glimpse(final_tiab) #take a peek
final_tiab %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble()) #check the NAs in dataframe
final_tiab <- final_tiab %>% relocate("database", .before = STUDY_ID) %>% relocate(c("jour_s", "num_authors"), .after=journal)

## Create dataframes for each decision category to organize; Leave out the "NO"s and remove some useless columns. 
fscreen_yes <- final_tiab %>% filter(INCLUDE %in% c("YES")) %>% 
          select(!c("REVIEWERS","volume","start_page")) 
fscreen_mby <- final_tiab %>% filter(INCLUDE %in% c("MAYBE")) %>% 
          select(!c("REVIEWERS","volume","start_page")) 
fscreen_rev <- final_tiab %>% filter(INCLUDE %in% c("REVIEW")) %>% 
          select(!c("REVIEWERS","volume","start_page")) 

### How many records per decision group have missing digital identifiers (DOI/URL)
fscreen_yes %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())
fscreen_mby %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())
fscreen_rev %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

### 2.0  Extract all the digital identifiers -----------------------------
### With just the YES for now
di_yes <- fscreen_yes %>% select(c("database", "label", "INCLUDE", "title", "jour_s", 
                                   "author","year", "doi_prv", "url_prv","doi_upd","url_upd"))
glimpse(di_yes)
### Combine the new and old doi/url columns
di_yes <- di_yes %>% mutate(doi = ifelse(is.na(doi_upd), doi_prv, doi_upd)) %>% 
                     mutate(url = ifelse(is.na(url_upd), url_prv, url_upd)) 
## clean missing and broken digital identifiers (urls and doi)
# fix url column
di_yes <- di_yes %>% mutate(url = ifelse(str_detect(url, "NA;"), NA, url))
## How many rows are still missing doi or url
di_yes %>% filter(is.na(doi)) %>% count() #65
di_yes %>% filter(is.na(url)) %>% count() #43 ##Urls for EACH of the database sources, seperated by ;

####DOI TO ZOTERO####### 
di_yes %>% filter(is.na(doi)) %>% View
di_yes %>% filter(!is.na(doi)) %>% count()
di_yes %>% filter(!is.na(doi)) %>% pull(doi)

write_csv(di_yes, file="yes_log.csv")

### 2.1 Download results straight from crossref----
### filter by entries that have fulltext available
# cr_y <- cr_works(dois = di_yes$doi, .progress="text")$data  
# ### What links are available to download that have a pdf?
# cr_y %>% unnest(link, names_repair = "check_unique") %>% filter(content.type == "application/pdf") %>% View

## Download from doi.org urls
####MEED TO MERGE WITH di_YES TO SEARCH
# di_yes %>%
#   filter(!is.na(cr_url)) %>%
#   mutate(pdflabel = paste0(label,".pdf")) %>% tibble %>%
#   get_and_save(data = ., links = "cr_url",
#                save_names = "pdflabel", dir = "./pdfs/yes",
#                delay = 15, print_every = 5, dupe_strategy = "ignore")
# ## Check which pdfs have already been downloaded
# di_yes %>%
#   filter(!is.na(oa_url)) %>%
#   mutate(pdflabel = paste0(label,".pdf")) %>% tibble %>%
#   check_for_existence(data = ., save_names = "pdflabel", dir = "./pdfs/yes")

### 2.2 Check availability on openaccess API-----------------------------------
### YES GROUP
oa_y <- oadoi_fetch(dois = di_yes$doi,
               email = "andrhabr@gmail.com",
               .progress = "text", .flatten = F) %>% mutate(decision = "YES") 
## What percentage is open-access?
oa_y %>% count(is_oa) %>% mutate(percent = n/nrow(oa_y)*100)
n_distinct(oa_y$doi) #how many 'unique' dois are there?

## let's get the links to the best available pdfs from the open-access articles on the list
glimpse(oa_y)
## 1st filter to open-access, 2nd unnest the oa url location, 3rd remove any duplicated rows
y <- oa_y %>% filter(is_oa==T) %>% unnest(best_oa_location, names_repair = "universal") %>% filter(!duplicated(doi))
# y2 <-oa_y %>% filter(is_oa==T) %>% unnest(oa_locations, names_repair = "universal")
my <- di_yes  %>% left_join(y %>% select(c("doi","url","url_for_pdf","url_for_landing_page","title")), 
                     by = "doi", suffix = c("_bib","_oa"))
glimpse(my)
## Download from OPEN-ACCESS BEST PDF URL
my %>%
  filter(!is.na(url_oa)) %>%
  mutate(pdflabel = paste0(label,".pdf")) %>% tibble %>%
  get_and_save(data = .,links = "url_oa",
               save_names = "pdflabel",dir = "./pdfs/yes",
               delay = 10, print_every=5, dupe_strategy = "ignore")
## Check which pdfs have already been downloaded
my %>%
  filter(!is.na(url_oa)) %>%
  mutate(pdflabel = paste0(label,".pdf")) %>% tibble %>%
  check_for_existence(data = ., save_names = "pdflabel",dir = "./pdfs/yes")

### MAYBE GROUP 
di_mby <- fscreen_mby %>% select(c("database", "label", "INCLUDE", "title", "jour_s", 
                                   "author", "doi_prv", "url_prv","doi_upd","url_upd"))
glimpse(di_yes)
### Combine the new and old doi/url columns
di_mby <- di_mby %>% mutate(doi = ifelse(is.na(doi_upd), doi_prv, doi_upd)) %>% 
  mutate(url = ifelse(is.na(url_upd), url_prv, url_upd)) 
## clean missing and broken digital identifiers (urls and doi)
# fix url column
di_mby <- di_mby %>% mutate(url = ifelse(str_detect(url, "NA;"), NA, url))
## How many rows are still missing doi or url
di_mby %>% filter(is.na(doi)) %>% count() #136
di_mby %>% filter(is.na(url)) %>% count() #69 ##Urls for EACH of the database sources, seperated by ;

## Check for open-access links
oa_m <- oadoi_fetch(dois = di_mby$doi,
                    email = "andrhabr@gmail.com",
                    .progress = "text", .flatten = F) %>% mutate(decision = "YES") 
## What percentage is open-access?
oa_m %>% count(is_oa) %>% mutate(percent = n/nrow(oa_m)*100)
oa_m %>% count(is.na(doi))
n_distinct(oa_m$doi) #how many 'unique' dois are there?

## 1st filter to open-access, 2nd unnest the oa url location, 3rd remove any duplicated rows
glimpse(oa_m)
m <- oa_m %>% filter(is_oa==T) %>% unnest(best_oa_location, names_repair = "universal") %>% filter(!duplicated(doi))
# y2 <-oa_y %>% filter(is_oa==T) %>% unnest(oa_locations, names_repair = "universal")
mm <- di_mby  %>% left_join(m %>% select(c("doi","url","url_for_pdf","url_for_landing_page","title")), 
                            by = "doi", suffix = c("_bib","_oa"))
glimpse(mm)
## Download from OPEN-ACCESS BEST PDF URL
mm %>%
  filter(!is.na(url_for_pdf)) %>%
  mutate(pdflabel = paste0(label,".pdf")) %>% tibble %>%
  get_and_save(data = .,links = "url_for_pdf",
               save_names = "pdflabel",dir = "./pdfs/maybe",
               delay = 10, print_every=5, dupe_strategy = "ignore")
## Check which pdfs have already been downloaded
mm %>%
  filter(!is.na(url_for_pdf)) %>%
  mutate(pdflabel = paste0(label,".pdf")) %>% tibble %>%
  check_for_existence(data = ., save_names = "pdflabel",dir = "./pdfs/maybe")


## X.1 Extract URLs from crossref ----
#### use DOIs to get info from crossref
cr_y <- cr_works(dois = doiy$doi, .progress="text")$data 
cr_m <- cr_works(dois = doim$doi, .progress="text")$data
cr_r <- cr_works(dois = doir$doi, .progress="text")$data 
cr_y %>% summarise(miss_url = sum(is.na(url)),
                   n_title = n_distinct(title),
                   n_jour = n_distinct(container.title))
glimpse(cr_y)
cr_url<-bind_rows(cr_y %>% select(doi, url),
                  cr_m %>% select(doi, url),
                  cr_r %>% select(doi, url))

bibs_urls <- bibs_urls %>% left_join(cr_url, by = "doi", suffix = c("_bib","_cr"))
#### Summary table of totals per group, with DOIs, and findable through crossref
tibble(Decision = c("YES", "MAYBE", "REVIEW"),
       Total = c(sum(bibs_clean$INCLUDE == "YES"),sum(bibs_clean$INCLUDE == "MAYBE"),sum(bibs_clean$INCLUDE == "REVIEW")),
       haveDOI = c(sum(!is.na(doiy$doi)), sum(!is.na(doim$doi)), sum(!is.na(doir$doi))),
       CrossRef = c(nrow(cr_y$data), nrow(cr_m$data), nrow(cr_r$data)))

## X.2 How many records are available open-access? ----

# Lets check to see availability of open-access pdfs (using oaDOI and unpaywall)
# Write the dataset to .csv to investigate the dataframe later. We *should* be able to extract open-access locations for the pdfs!

####FLATTEN MAKES IT A TIDY DATAFRAME, flatten=F makes it a tdl_df with lists stored in the table
# y<-oadoi_fetch(dois = cr_y$data$doi,
#             email = "andrhabr@gmail.com",
#             .progress = "text",.flatten = F) %>% mutate(decision = "YES")
# m<-oadoi_fetch(dois = cr_m$data$doi,
#                email = "andrhabr@gmail.com",
#                .progress = "text",.flatten = F) %>% mutate(decision = "MAYBE")
# r<-oadoi_fetch(dois = cr_r$data$doi,
#                email = "andrhabr@gmail.com",
#                .progress = "text",.flatten = F) %>% mutate(decision = "REVIEW")
#oa_dat <- bind_rows(y,m,r) #bind them to one dataframe

###
# write_csv(oa_dat, "./data/article_oa_availability-05.csv", col_names = T) #write to csv, uncomment to save
#oa_dat <- read_csv("./data/article_oa_availability-05.csv", col_names = T)
glimpse(oa_dat) #take a peek at the columns

# Let's remove some of the useless columns so it is less cluttered.
oa_clean <- oa_dat %>% select(-c("updated","host_type","license","version","oa_date","repository_institution",
                                 "pmh_id","endpoint_id","is_paratext","journal_is_in_doaj","published_date",
                                 "updated_resource")) 

oa_clean %>% filter(decision == "YES") %>% 
  group_by(doi) %>% 
  summarise(num_na = sum(is.na(url))) %>% 
  filter(num_na > 0) %>% 
  tally()
# What are the number unique values in each of the columns, by decision group? What does this tell us about the dataframe?
oa_clean %>% group_by(decision) %>% count(is_oa, is_best)
oa_clean %>% group_by(decision) %>% summarize(num_distinct_dois = n_distinct(doi))
# These are the DOIs for the categories that can be found through open-access DOI...Now we gotta download them...
# extract list of urls that are open-access
oa_url<- oa_clean %>% filter(is_oa == "TRUE" & is_best == "TRUE")

### merge dataframes to keep track of downloads
bib_url <- bibs_clean %>%
  left_join(oa_url %>% select(doi, url_for_pdf), by = "doi")

## 3.2 Extract URLs from crossref ----
#### use DOIs to get info from crossref
cr_y <- cr_works(dois = doiy$doi, .progress="text")$data 
cr_m <- cr_works(dois = doim$doi, .progress="text")$data
cr_r <- cr_works(dois = doir$doi, .progress="text")$data 
cr_y %>% summarise(miss_url = sum(is.na(url)),
                n_title = n_distinct(title),
                n_jour = n_distinct(container.title))
glimpse(cr_y)
cr_url<-bind_rows(cr_y %>% select(doi, url),
          cr_m %>% select(doi, url),
          cr_r %>% select(doi, url))

bibs_urls <- bibs_urls %>% left_join(cr_url, by = "doi", suffix = c("_bib","_cr"))
#### Summary table of totals per group, with DOIs, and findable through crossref
tibble(Decision = c("YES", "MAYBE", "REVIEW"),
       Total = c(sum(bibs_clean$INCLUDE == "YES"),sum(bibs_clean$INCLUDE == "MAYBE"),sum(bibs_clean$INCLUDE == "REVIEW")),
       haveDOI = c(sum(!is.na(doiy$doi)), sum(!is.na(doim$doi)), sum(!is.na(doir$doi))),
       CrossRef = c(nrow(cr_y$data), nrow(cr_m$data), nrow(cr_r$data)))


