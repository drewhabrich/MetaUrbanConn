# 1. Load relevant packages into library -------------------------------------
#install.packages("tidyverse","metagear","PRISMA2020","rcrossref) install packages as needed
library(plyr) #v1.8.8; plyr needs to be loaded BEFORE tidyverse to prevent issues with dplyr
library(tidyverse) #v1.3.2
library(synthesisr) #v0.3.0
library(revtools) #v0.4.1
library(lubridate) #v1.9.1; this should be in the tidyverse, if for some reason it isn't you can install it separately

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
initial_dat<-read_bibliography(filename="./output/deduplicated_bib_02-1.ris", return_df=T)

# let's check the structure and clean the data so that we can systematically screen
str(initial_dat) #all columns are 'character'
ncol(initial_dat) #16 columns of data

# 2.1. Data cleaning and exploration --------------------------------------
#How many of the columns are *mostly* empty
initial_dat %>% 
  summarise(
    across(everything(), ~ sum(is.na(.))) %>% as_tibble()
  )
# remove the unnecessary columns...
initial_dat <- initial_dat %>% select(-c(keywords,n1))

# Check the entries that are missing their titles, author, year, doi, abstract
initial_dat %>% filter(is.na(title)) %>% as_tibble() #NONE!
initial_dat %>% filter(is.na(author)) %>% as_tibble()
initial_dat %>% filter(is.na(year)) %>% as_tibble()

initial_dat <- initial_dat %>% mutate(year=as.numeric(year)) %>% as_tibble() #change year to be numeric instead of character
#save a seperate dataframe to work with
dat<-initial_dat 

# How many empty columns are there?
dat %>% 
  summarise(
    across(everything(), ~ sum(is.na(.))) %>% as_tibble()
  )

# Is there a pattern to which entries are missing DOI? Or URLS?
## 2.1.1 DOI MISSING
dat %>% filter(is.na(doi)) %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "Search results with missing DOI by year") + xlim(1980,NA)
# where are these entries missing other data?
dat %>% filter(is.na(doi)) %>% summarise(across(everything(), ~ sum(is.na(.)))) #all missing doi are missing the journal entry too...
dat %>% filter(is.na(doi)) %>% group_by(journal) %>% count(journal, sort=T) 
#Results from PROQUEST put the source here instead of 'Journal' likely because there are a number of theses/dissertations that weren't published in a peer-reviewed journal
# visualize frequencies with a plot 
dat %>% filter(is.na(doi)) %>% group_by(journal) %>% count(journal, sort=T) %>% 
  ggplot(aes(y = reorder(journal, n), x=n)) + #reorder the y-axis by the n column (frequency)
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Source", title = "Frequency of entries by source") +
  theme(axis.text.y = element_text(size=5, hjust=1))

## 2.1.2 Journal MISSING
# What entries are STILL missing journal info?
dat %>% filter(is.na(journal)) %>% count(type) #139 that are entered as JOUR!
dat %>% filter(is.na(journal)) %>% filter(type=="JOUR") %>% summarise(sum(is.na(url))) #39 entries don't have a URL, the others are from SCOPUS
dat %>% filter(is.na(journal)) %>% filter(type=="JOUR") %>% summarise(sum(is.na(doi))) #all are missing their DOIs

## 2.1.3 URL MISSING
dat %>% filter(is.na(url)) %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "Search results with missing URLS by year") + xlim(1980,NA) #HUGE gap in 2022, why is that...

dat %>% filter(is.na(url)) %>% filter(year=="2022") #They have DOIs so its fine if missing URL
# where are these entries missing other data?
dat %>% filter(is.na(url)) %>% summarise(across(everything(), ~ sum(is.na(.)))) #all missing doi are missing the journal entry too...

# 2.2 FILL MISSING BIBLIOGRAPHIC DATA ---------------------------------------
# Can we populate the missing cells based on doi, title, author/year using 'crossref'?
# install.packages("crossref") #uncomment if install is needed
library(rcrossref) #v1.2.0
dat %>% filter(is.na(doi)) %>% nrow() #682 rows missing DOI

# fill missing years based on doi
year_doi<- dat %>% filter(is.na(year)) %>% select(doi,title) #filter only the entries with missing year
year_d<-cr_works(dois=year_doi$doi)$data #extract data from crossref based on  year

year_doi$year <- year(ymd(year_d$published.online)) #classify as date and extract the year

dat <- dat %>%
  left_join(year_doi %>% select(doi, year), by = "doi") %>% #
  mutate(year = if_else(is.na(year.x), year.y, year.x)) %>% #
  select(-year.x, -year.y) %>% #remove the redundant columns
  relocate(year, .after = author) #place year column after author column

dat %>% 
  summarise(
    across(everything(), ~ sum(is.na(.))) %>% as_tibble()
  )

# 3 Bibliography data exploration -----------------------------------------
# data structure
head(dat)
str(dat)
dat %>% summarise(across(everything(), ~ sum(is.na(.)))) #Columns with # of NAs 

# how many publications by year are there?
dat %>% ggplot(aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "Year", y = "Publication count", title = "'Urban landscape connectivity' search results by year") + xlim(1990,2024)

# how many authors are there?
n_distinct(dat$author) #this is only a DIRECT match.
# Count the number of authors in each row, handling both ";" and "and" as separators; NOTE: this uses regular expression to find text strings
dat$num_authors <- sapply(strsplit(dat$author, ";\\s*|\\s+and\\s+"), length)
# what is the range of # of authors
dat %>% ggplot(aes(x = num_authors)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(x = "# of authors on publication", y = "Frequency") + xlim(1,30)

dat %>% summarise(mean_numauth=mean(num_authors),
                  med_numauth=median(num_authors),
                  min_numauth=min(num_authors),
                  max_numauth=max(num_authors)) #the entry with the max # of authors is a dataset publication with 199 people!

# how many journals are entries published in (NOTE: some entries do not have a journal; i.e. preprint, thesis, or non-peer reviewed articles)
n_distinct(dat$journal) #distinct journal entries (SOME MAY BE DUPLICATEDDD, this only matches on EXACT)
dat %>% 
  group_by(journal) %>% 
  count(journal, sort=T) %>% 
  ggplot(aes(x=n, y = reorder(journal,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme(axis.text.y = element_text(size=5))


# 4 screening -------------------------------------------------------------
bibdat<-as.data.frame(dat)
screen_topics(bibdat)
screen
