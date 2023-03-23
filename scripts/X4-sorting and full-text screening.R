#' ---
#' title: "04-Cleaning bibliography for pdf downloads for full-textscreening"
#' author: "Andrew Habrich"
#' date: '`r Sys.Date()`'
#' output: 
#'  html_document: 
#'   toc: yes
#'   toc_float:
#'     collapsed: yes
#'     smooth_scroll: yes
#'   fig_caption: yes
#'   code_folding: hide
#'   code_download: yes
#'   keep_md: true
#' ---

#'# 1. Load required packages
#+ packages, echo=T, message=F
# 1. Load packages ------------------------------------------------
## install.packages("tidyverse","metagear","PRISMA2020","rcrossref) install packages as needed
library(tidyverse) #v2.0.0
library(metagear) #v0.7.0
library(rcrossref) #v1.2.0
library(lubridate)
## packages still in-development 
library(roadoi) #v0.7.2 Find Free Versions of Scholarly Publications via Unpaywall
library(heapsofpapers) #v0.1.0 download pdfs from URL
library(DataEditR) #v0.1.5, testing this package, trying to type less as accomodation for tendinosis

## potentially interesting packages on RopenSci
## heapsofpapers - download pdfs from url
## roadoi - Find Free Versions of Scholarly Publications via Unpaywall
## medrxivr - Access and Search MedRxiv and BioRxiv Preprint Data
## pdftools - Text Extraction, Rendering and Converting of PDF Documents
#'
#'# 2. Import dataframe and tidy 
#+ import-tidy, echo=F, 
# 2. Import and cleaning-----------------------------------------------------------
bibs<-effort_merge(directory="./output")
#' Generate a summary table by each decision category
bibs %>% 
  group_by(INCLUDE) %>% #group by category
  summarize(count=n()) %>% #summarize based on count of each category
  mutate(percentage = count/nrow(bibs)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOTconn" ~ "Excluded based title, not landscape connectivity",
                             INCLUDE == "NOTurban" ~ "Excluded based on abstract, not urban/city",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage)) #arrange in descending % order
#'
#' How many rows in each of the columns are empty?
#+ count rowNA, echo=F
bibs %>% group_by(INCLUDE) %>% relocate("source", .after="journal") %>% 
  summarise(across(everything(), ~ sum(is.na(.))) %>% tibble())

#'## 2.1 Fill empty cells in dataframe
## 2.1 Tidy ----
#' - Fix journal names
#' - Fill empty cells in the "journal" column with the corresponding values from the "source" column. This is an artifact from SCOPUS and ProQuest having different column names during export
#+ cleaning, echo=F, include=F
bibs <- bibs %>%
  mutate(journal = if_else(is.na(journal), source, journal))
#' - Fill the last remaining empty 'journals' with the source type if it is not a journal (e.g. report, book, or conference paper)
bibs <- bibs %>% 
  mutate(journal = if_else(is.na(journal), source_type, journal),
         author = if_else(is.na(author),"MISSING", author))

#' - Force journal names to title-case 
bibs <- bibs%>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(jour_s = str_to_lower(journal)) %>%
  mutate(jour_s = str_to_title(jour_s)) %>% #coerce to title-case
  mutate(jour_s = str_replace_all(jour_s, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

#' - Fix any missing 'year' data 
#+ fill missing years based on doi
bibs$year <- as.numeric(bibs$year) 
year_doi <- bibs%>% filter(is.na(year)) %>% select(doi, title, url) #filter only the entries with missing year
year_d <- cr_works(dois = year_doi$doi)$data #extract data from crossref based on  year
year_doi$year <- year(ymd(year_d$published.online)) #classify as date and extract the year
year_doi$url <- year_d$url

# join dataframes by doi and remove the redundant year and url columns
bibs <- bibs %>%
  left_join(year_doi %>% select(doi, year, url), by = "doi") %>% #
  mutate(year = if_else(is.na(year.x), year.y, year.x)) %>% #
  mutate(url = if_else(is.na(url.x), url.y, url.x)) %>%
  select(-year.x, -year.y, -url.x, -url.y) %>% #remove the redundant columns
  relocate(year, .after = author) 

#'### Check dataframe structure
#' Let's exclude the entries eliminated based on title/abstract and remove some unneccesary columns to minimize clutter. 
bibs_clean <- bibs %>% filter(INCLUDE %in% c("YES", "MAYBE", "REVIEW")) %>% 
  select(!c("REVIEWERS","publisher","notes","journal","source")) %>% relocate(jour_s, .after = title)
#' There are {{nrow(bibs_clean)}} entries in these categories. 

#+ summary tables, echo=F
#' Take a glimpse of the column structure and the first couple of rows of the dataframe
glimpse(bibs_clean)
bibs_clean %>% as_tibble()
#' Everything looks good so far, but how many entries per decision group are missing doi/url (for digital retrieval)? 
bibs_clean %>% group_by(INCLUDE) %>% 
        summarize(count = n(),
                  na_doi = sum(is.na(doi)),
                  d_Pmiss = sum(is.na(doi))/n()*100,
                  na_url = sum(is.na(url)),
                  u_Pmiss = sum(is.na(url))/n()*100)

#' How many columns still have missing values?
bibs_clean %>% summarise(
  across(everything(), ~ sum(is.na(.))) %>% as_tibble())

#'## 2.2 Create labels for each entry 
#+ labels, results='hide'
## 2.2 Create labels for pdf naming ----
### create a studyID column that is author/year to name pdfs with
#### count the number of authors in each row, handling both ";" and "and" as separators; NOTE: uses regex to find text strings
bibs_clean$num_authors <- sapply(strsplit(bibs_clean$author, ";\\s*|\\s+and\\s+"), length)
#### create label column in the format: Author_year_journal
bibs_clean <- bibs_clean %>%
  mutate(label = ifelse(num_authors > 2,#condition
      str_c(word(author, 1, sep = ", "),"et_al", year, str_replace_all(jour_s, " ", ""),sep = "_"),#if condition is met
      str_c(word(author, 1, sep = ", "), year, str_replace_all(jour_s, " ", ""), sep = "_" #else use this
            )) 
      ) %>%  
      mutate(label = str_trunc(label, width = 50, side = c("right"), ellipsis = "")) %>%
      relocate(label, .after = "STUDY_ID")
    
 bibs_clean %>% filter(STUDY_ID %in% c("1320","803","952")) %>% .$label #check to see if it did what we want
 bibs_clean %>% filter(STUDY_ID %in% c("1320","803","952")) %>% .$author #compare with author column to see if it worked

### for each decision group create a dataframe (with doi) 
doiy <- bibs_clean %>% filter(INCLUDE == 'YES') %>% filter(!is.na(doi))
doim <- bibs_clean %>% filter(INCLUDE == 'MAYBE') %>% filter(!is.na(doi))
doir <- bibs_clean %>% filter(INCLUDE == 'REVIEW') %>% filter(!is.na(doi))

### how many rows are missing for each dataframe
bind_rows(doiy %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble()) %>% mutate(source="doiYES", .before=label),
          doim %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble()) %>% mutate(source="doiMAY", .before=label),
          doir %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble()) %>% mutate(source="doiREV", .before=label)
)

#'## 2.3 Prepare dataframe
## 2.3 Prepare dataframe ---------------------------------------------------

#### use DOIs to get info from crossref
# cr_y <- cr_works(dois = doiy$doi, .progress="text") #12 entries not found
# cr_m <- cr_works(dois = doim$doi, .progress="text") #35 entries not found
# cr_r <- cr_works(dois = doir$doi, .progress="text") #12 entries not found
# 
# #### Summary table of totals per group, with DOIs, and findable through crossref
# tibble(decision = c("YES", "MAYBE", "REVIEW"),
#        Total = c(sum(bibs_clean$INCLUDE == "YES"),sum(bibs_clean$INCLUDE == "MAYBE"),sum(bibs_clean$INCLUDE == "REVIEW")),
#        haveDOI = c(nrow(doiy), nrow(doim), nrow(doir)),
#        onCrossRef = c(nrow(cr_y$data), nrow(cr_m$data), nrow(cr_r$data)))

#' Lets check to see availability of open-access pdfs (using oaDOI and unpaywall)
#' Write the dataset to .csv to investigate the dataframe later. We *should* be able to extract open-access locations for the pdfs!

# y<-oadoi_fetch(dois = cr_y$data$doi,
#             email = "andrhabr@gmail.com",
#             .progress = "text",.flatten = T)
# y <- y %>% mutate(decision = "YES")
# m<-oadoi_fetch(dois = cr_m$data$doi,
#                email = "andrhabr@gmail.com",
#                .progress = "text",.flatten = T)
# m <- m %>% mutate(decision = "MAYBE")
# r<-oadoi_fetch(dois = cr_r$data$doi,
#                email = "andrhabr@gmail.com",
#                .progress = "text",.flatten = T)
# r <- r %>% mutate(decision = "REVIEW")
# oa_avail <- bind_rows(y,m,r)

## write_csv(oa_avail, "./data/article_oa_availability-0X.csv", col_names = T)
#+ oa-avail, echo=F
oa_dat <- read_csv("./data/article_oa_availability-0X.csv", col_names = T)
glimpse(oa_dat) #take a peek at the columns
# Let's remove some of the useless columns so it is less cluttered.
oa_clean <- oa_dat %>% select(-c("updated","host_type","license","version","oa_date","repository_institution",
                                 "pmh_id","endpoint_id","is_paratext","journal_is_in_doaj","published_date",
                                 "updated_resource")) 
#'
#' What are the number unique values in each of the columns, by decision group? What does this tell us about the dataframe?
#+ openaccess summary
oa_clean %>% group_by(decision) %>% count(is_oa, is_best)
oa_clean %>% group_by(decision) %>% summarize(num_distinct_dois = n_distinct(doi))
#' These are the DOIs for the categories that can be found through open-access DOI...Now we gotta download them...
# extract list of urls that are open-access
oa_url<- oa_clean %>% filter(is_oa == "TRUE" & is_best == "TRUE")
  
### merge dataframes to keep track of downloads
bib_url <- bibs_clean %>%
  left_join(oa_url %>% select(doi, url_for_pdf), by = "doi")
#'

#'# 3. Download PDFs
# 3. Download PDFs --------------------------------------------------------
#+
## 3.1 using 'heapsofpapers' to download from URL ----
bib_url %>%
  filter(!is.na(url_for_pdf)) %>%
  filter(INCLUDE == "YES") %>% count()
### check how many have already been downloaded
bib_url %>%
  filter(!is.na(url_for_pdf)) %>%
  filter(INCLUDE == "YES") %>%
  mutate(label = paste0(label,".pdf")) %>%
  tibble %>% 
  check_for_existence(data = ., 
                    save_names = "label",
                    dir = "./pdfs")
### uncomment to download pdfs from url

# bib_url %>% 
#   filter(!is.na(url_for_pdf)) %>% 
#   filter(INCLUDE == "YES") %>% 
#   mutate(label = paste0(label,".pdf")) %>%
#   tibble %>% 
#   get_and_save(data = .,links = "url_for_pdf", save_names = "label", dir = "./pdfs", dupe_strategy = "overwrite")

###CAN I SAVE THE CR DATALISTS TO A CSV? #HOW TO AVOID CONSTANTLY CHECKING
#### make a test dataframe with 10 random rows from the dataset
# set.seed(1010) #set seed to be replicable across sessions
# doiy10 <- sample_n(doiy, 10)
# 
# ## 3.2 using metagear download feature (DOI) ----
# library(metagear)
# # try with 1 doi only
# # x <-PDF_download(doiy$doi[1], theFileName = "temp", validatePDF = TRUE, directory = "./pdfs",
# #             quiet = FALSE, WindowsProxy = TRUE)
# # 
# xy <- PDFs_collect(doiy10, 
#                        DOIcolumn = "doi", 
#                        FileNamecolumn = "label",
#                        directory = "./pdfs",
#                        quiet = F, validatePDF = T, WindowsProxy = T, showSummary = T, buffer = T)
# ###Corrupted pdfs downloaded from here... curious what other options exist
# 
# ## 3.3 using RCurl ----
# library(rcrossref)
# library(RCurl)
# library(purrr)
# 
# df2 <- df %>% as.data.frame() 
# lapply(1:nrow(df2), \(x) {
#   if (url.exists(df2[x, "URL"])) download.file(df2[x, "URL"], paste0(df2[x, "Name"], "_", df2[x, "Ticker"], ".pdf"), mode="wb")
# }) ##apply function to list, if url is valid