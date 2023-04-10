---
title: "04-Cleaning bibliography for pdf downloads for full-textscreening"
author: "Andrew Habrich"
date: '2023-03-22'
output: 
 html_document: 
  toc: yes
  toc_float:
    collapsed: yes
    smooth_scroll: yes
  fig_caption: yes
  code_folding: hide
  code_download: yes
  keep_md: true
---

# 1. Load required packages

``` r
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
```

# 2. Import dataframe and tidy

Generate a summary table by each decision category

``` r
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
```

```         
## # A tibble: 5 × 4
##   INCLUDE  count percentage summary                                         
##   <chr>    <int>      <dbl> <chr>                                           
## 1 NOTconn   1635      45.8  Excluded based title, not landscape connectivity
## 2 NOTurban   695      19.5  Excluded based on abstract, not urban/city      
## 3 MAYBE      610      17.1  Unclear eligibility, needs full text screening  
## 4 YES        331       9.27 candidate studies identified                    
## 5 REVIEW     299       8.38 Review/Methodology/Framework articles
```

How many rows in each of the columns are empty?

```         
## # A tibble: 5 × 18
##   INCLUDE STUDY…¹ REVIE…² sourc…³ author  year title journal source volume issue
##   <chr>     <int>   <int>   <int>  <int> <int> <int>   <int>  <int>  <int> <int>
## 1 MAYBE         0       0       0      0     1     0     167    428    123   225
## 2 NOTconn       0       0       0     24     2     0     857    823    698   851
## 3 NOTurb…       0       0       0      0     0     0     241    465    205   275
## 4 REVIEW        0       0       0      9     0     0      92    221     76   113
## 5 YES           0       0       0      0     3     0      52    237     44    96
## # … with 7 more variables: start_page <int>, end_page <int>, abstract <int>,
## #   doi <int>, url <int>, publisher <int>, notes <int>, and abbreviated
## #   variable names ¹​STUDY_ID, ²​REVIEWERS, ³​source_type
```

## 2.1 Fill empty cells in dataframe

``` r
## 2.1 Tidy ----
```

-   Fix journal names

-   Fill empty cells in the "journal" column with the corresponding values from the "source" column. This is an artifact from SCOPUS and ProQuest having different column names during export

-   Fill the last remaining empty 'journals' with the source type if it is not a journal (e.g. report, book, or conference paper)

``` r
bibs <- bibs %>% 
  mutate(journal = if_else(is.na(journal), source_type, journal),
         author = if_else(is.na(author),"MISSING", author))
```

-   Force journal names to title-case

``` r
bibs <- bibs%>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(jour_s = str_to_lower(journal)) %>%
  mutate(jour_s = str_to_title(jour_s)) %>% #coerce to title-case
  mutate(jour_s = str_replace_all(jour_s, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase
```

-   Fix any missing 'year' data

``` r
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
```

### Check dataframe structure

Let's exclude the entries eliminated based on title/abstract and remove some unneccesary columns to minimize clutter.

``` r
bibs_clean <- bibs %>% filter(INCLUDE %in% c("YES", "MAYBE", "REVIEW")) %>% 
  select(!c("REVIEWERS","publisher","notes","journal","source")) %>% relocate(jour_s, .after = title)
```

There are {{nrow(bibs_clean)}} entries in these categories.

Take a glimpse of the column structure and the first couple of rows of the dataframe

``` r
glimpse(bibs_clean)
```

```         
## Rows: 1,240
## Columns: 14
## $ STUDY_ID    <int> 2, 4, 5, 6, 8, 9, 12, 15, 17, 19, 20, 21, 23, 24, 27, 29, …
## $ INCLUDE     <chr> "YES", "YES", "REVIEW", "REVIEW", "REVIEW", "MAYBE", "REVI…
## $ source_type <chr> "JOUR", "JOUR", "JOUR", "JOUR", "JOUR", "JOUR", "JOUR", "J…
## $ author      <chr> "Adams, Tempe S. F. and Chase, Michael J. and Rogers, Trac…
## $ year        <dbl> 2017, 2014, 2020, 2020, 2020, 2017, 2018, 2020, 2020, 1993…
## $ title       <chr> "Taking the elephant out of the room and into the corridor…
## $ jour_s      <chr> "Oryx", "Ecology", "Conservation Science and Practice", "L…
## $ volume      <chr> "51", "95", "2", "9", "251", "72", "46", "35", NA, "8", "2…
## $ issue       <chr> "2", "4", "6", "8", NA, NA, NA, "8", NA, "1", "6", "9", "9…
## $ start_page  <chr> "347", "1010", NA, "239", "108679", "895", "38", "1809", N…
## $ end_page    <chr> "353", "1021", NA, NA, NA, "909", "47", "1825", NA, "37", …
## $ abstract    <chr> "Transfrontier wildlife corridors can be successful conser…
## $ doi         <chr> "10.1017/S0030605315001246", "10.1890/13-0705.1", "10.1111…
## $ url         <chr> "https://www.scopus.com/inward/record.uri?eid=2-s2.0-84965…
```

``` r
bibs_clean %>% as_tibble()
```

```         
## # A tibble: 1,240 × 14
##    STUDY_ID INCLUDE source_type author    year title jour_s volume issue start…¹
##       <int> <chr>   <chr>       <chr>    <dbl> <chr> <chr>  <chr>  <chr> <chr>  
##  1        2 YES     JOUR        Adams, …  2017 Taki… Oryx   51     2     347    
##  2        4 YES     JOUR        Braaker…  2014 Habi… Ecolo… 95     4     1010   
##  3        5 REVIEW  JOUR        Belote,…  2020 Deli… Conse… 2      6     <NA>   
##  4        6 REVIEW  JOUR        Castill…  2020 Conn… Land   9      8     239    
##  5        8 REVIEW  JOUR        Cao, Yu…  2020 Link… Biolo… 251    <NA>  108679 
##  6        9 MAYBE   JOUR        Correa …  2017 Anth… Ecolo… 72     <NA>  895    
##  7       12 REVIEW  JOUR        Duflot,…  2018 Comb… Journ… 46     <NA>  38     
##  8       15 MAYBE   JOUR        Ghoddou…  2020 Mapp… Lands… 35     8     1809   
##  9       17 REVIEW  BOOK        Hilty, …  2020 Guid… Book   <NA>   <NA>  <NA>   
## 10       19 MAYBE   JOUR        La Poll…  1993 Effe… Lands… 8      1     25     
## # … with 1,230 more rows, 4 more variables: end_page <chr>, abstract <chr>,
## #   doi <chr>, url <chr>, and abbreviated variable name ¹​start_page
```

Everything looks good so far, but how many entries per decision group are missing doi/url (for digital retrieval)?

``` r
bibs_clean %>% group_by(INCLUDE) %>% 
        summarize(count = n(),
                  na_doi = sum(is.na(doi)),
                  d_Pmiss = sum(is.na(doi))/n()*100,
                  na_url = sum(is.na(url)),
                  u_Pmiss = sum(is.na(url))/n()*100)
```

```         
## # A tibble: 3 × 6
##   INCLUDE count na_doi d_Pmiss na_url u_Pmiss
##   <chr>   <int>  <int>   <dbl>  <int>   <dbl>
## 1 MAYBE     610     98    16.1     23    3.77
## 2 REVIEW    299     46    15.4     18    6.02
## 3 YES       331     39    11.8     19    5.74
```

How many columns still have missing values?

``` r
bibs_clean %>% summarise(
  across(everything(), ~ sum(is.na(.))) %>% as_tibble())
```

```         
##   STUDY_ID INCLUDE source_type author year title jour_s volume issue start_page
## 1        0       0           0      0    0     0      0    243   434        166
##   end_page abstract doi url
## 1      419       40 183  60
```

## 2.2 Create labels for each entry

``` r
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
```

## 2.3 Prepare dataframe

``` r
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
```

Lets check to see availability of open-access pdfs (using oaDOI and unpaywall) Write the dataset to .csv to investigate the dataframe later. We *should* be able to extract open-access locations for the pdfs!

``` r
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

## write_csv(oa_avail, "./output/article_oa_availability-04.csv", col_names = T)
```

```         
## Rows: 1477 Columns: 31
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (19): doi, url, url_for_pdf, url_for_landing_page, evidence, license, v...
## dbl   (2): data_standard, year
## lgl   (6): is_best, is_oa, is_paratext, has_repository_copy, journal_is_oa, ...
## dttm  (2): updated, updated_resource
## date  (2): oa_date, published_date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```         
## Rows: 1,477
## Columns: 31
## $ doi                    <chr> "10.1017/s0030605315001246", "10.1890/13-0705.1…
## $ updated                <dttm> 2020-09-30 13:12:09, 2022-12-01 17:55:01, NA, …
## $ url                    <chr> "https://www.cambridge.org/core/services/aop-ca…
## $ url_for_pdf            <chr> "https://www.cambridge.org/core/services/aop-ca…
## $ url_for_landing_page   <chr> "https://doi.org/10.1017/s0030605315001246", "h…
## $ evidence               <chr> "open (via page says license)", "oa repository …
## $ license                <chr> "cc-by", NA, NA, "cc-by", "cc-by", NA, NA, NA, …
## $ version                <chr> "publishedVersion", "acceptedVersion", NA, "pub…
## $ host_type              <chr> "publisher", "repository", NA, "publisher", "re…
## $ is_best                <lgl> TRUE, TRUE, NA, TRUE, FALSE, NA, NA, NA, TRUE, …
## $ pmh_id                 <chr> NA, "oai:dora:wsl_4891", NA, NA, NA, NA, NA, NA…
## $ endpoint_id            <chr> NA, "97aa526343b06bea6c9", NA, NA, NA, NA, NA, …
## $ repository_institution <chr> NA, "Swiss Federal Institute for Forest, Snow a…
## $ oa_date                <date> 2016-05-06, NA, NA, 2022-04-08, 2022-01-31, NA…
## $ data_standard          <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
## $ is_oa                  <lgl> TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FA…
## $ is_paratext            <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
## $ genre                  <chr> "journal-article", "journal-article", "journal-…
## $ oa_status              <chr> "hybrid", "green", "closed", "gold", "gold", "c…
## $ has_repository_copy    <lgl> FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, F…
## $ journal_is_oa          <lgl> FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, …
## $ journal_is_in_doaj     <lgl> FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, …
## $ journal_issns          <chr> "0030-6053,1365-3008", "0012-9658", "1466-822X,…
## $ journal_issn_l         <chr> "0030-6053", "0012-9658", "1466-822X", "2051-39…
## $ journal_name           <chr> "Oryx", "Ecology", "Global Ecology and Biogeogr…
## $ publisher              <chr> "Cambridge University Press (CUP)", "Wiley", "W…
## $ published_date         <date> 2016-05-06, 2014-04-01, 2018-07-04, 2022-04-08…
## $ year                   <dbl> 2016, 2014, 2018, 2022, 2022, 2015, 2017, 2016,…
## $ title                  <chr> "Taking the elephant out of the room and into t…
## $ updated_resource       <dttm> 2021-01-14 21:59:55, 2021-01-20 17:10:18, 2022…
## $ decision               <chr> "YES", "YES", "YES", "YES", "YES", "YES", "YES"…
```

What are the number unique values in each of the columns, by decision group? What does this tell us about the dataframe?

``` r
oa_clean %>% group_by(decision) %>% count(is_oa, is_best)
```

```         
## # A tibble: 9 × 4
## # Groups:   decision [3]
##   decision is_oa is_best     n
##   <chr>    <lgl> <lgl>   <int>
## 1 MAYBE    FALSE NA        241
## 2 MAYBE    TRUE  FALSE     187
## 3 MAYBE    TRUE  TRUE      236
## 4 REVIEW   FALSE NA        101
## 5 REVIEW   TRUE  FALSE     149
## 6 REVIEW   TRUE  TRUE      140
## 7 YES      FALSE NA        147
## 8 YES      TRUE  FALSE     143
## 9 YES      TRUE  TRUE      133
```

``` r
oa_clean %>% group_by(decision) %>% summarize(num_distinct_dois = n_distinct(doi))
```

```         
## # A tibble: 3 × 2
##   decision num_distinct_dois
##   <chr>                <int>
## 1 MAYBE                  476
## 2 REVIEW                 241
## 3 YES                    279
```

These are the DOIs for the categories that can be found through open-access DOI...Now we gotta download them...

``` r
# extract list of urls that are open-access
oa_url<- oa_clean %>% filter(is_oa == "TRUE" & is_best == "TRUE")
  
### merge dataframes to keep track of downloads
bib_url <- bibs_clean %>%
  left_join(oa_url %>% select(doi, url_for_pdf), by = "doi")
```

# 3. Download PDFs

``` r
# 3. Download PDFs --------------------------------------------------------
```

``` r
## 3.1 using 'heapsofpapers' to download from URL ----
bib_url %>%
  filter(!is.na(url_for_pdf)) %>%
  filter(INCLUDE == "YES") %>% count()
```

```         
##    n
## 1 97
```

``` r
### check how many have already been downloaded
bib_url %>%
  filter(!is.na(url_for_pdf)) %>%
  filter(INCLUDE == "YES") %>%
  mutate(label = paste0(label,".pdf")) %>%
  tibble %>% 
  check_for_existence(data = ., 
                    save_names = "label",
                    dir = "./pdfs")
```

```         
## [1] "You already have 95 of these. Consider filtering before running `get_and_save()` or running `get_and_save()` with dupe_strategy = 'ignore'."
```

```         
## # A tibble: 97 × 19
##    STUDY_ID label INCLUDE sourc…¹ author  year title jour_s volume issue start…²
##       <int> <chr> <chr>   <chr>   <chr>  <dbl> <chr> <chr>  <chr>  <chr> <chr>  
##  1        4 Braa… YES     JOUR    Braak…  2014 Habi… Ecolo… 95     4     1010   
##  2      134 Whit… YES     RPRT    Whitt…  2022 Town… Movem… 10     1     n/a    
##  3      273 Brod… YES     JOUR    Brodi…  2015 Eval… Conse… 29     1     122    
##  4      283 Albe… YES     JOUR    Alber…  2017 Appl… Conse… 31     6     1383   
##  5      283 Albe… YES     JOUR    Alber…  2017 Appl… Conse… 31     6     1383   
##  6      361 Garf… YES     JOUR    Garfi…  2022 Powe… Susta… 14     12    7113   
##  7      384 Hale… YES     JOUR    Hale,…  2015 The … Globa… 21     7     2467   
##  8      413 Lamp… YES     JOUR    Lampi…  2015 Urba… Plos … 10     11    e01422…
##  9      441 Mime… YES     RPRT    Mimet…  2019 Cont… Biorx… <NA>   <NA>  579227 
## 10      447 Olej… YES     JOUR    Olejn…  2018 Urba… Journ… 4      1     <NA>   
## # … with 87 more rows, 8 more variables: end_page <chr>, abstract <chr>,
## #   doi <chr>, url <chr>, num_authors <int>, url_for_pdf <chr>,
## #   save_names_full_path <fs::path>, got_this_already <dbl>, and abbreviated
## #   variable names ¹​source_type, ²​start_page
```

``` r
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
```
