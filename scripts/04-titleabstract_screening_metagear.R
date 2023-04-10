# 1. Load relevant packages into library -------------------------------------
## install.packages("tidyverse","metagear","PRISMA2020","rcrossref) install packages as needed
library(plyr) #plyr needs to be loaded BEFORE tidyverse to prevent issues with dplyr
library(tidyverse) #v2.0.0
library(metagear) #v0.7.0

# 2. Read in the bibliographic data set ------------------------------------
# rm(list=ls()) #remove everything in the R environment, use as needed.
initial_dat <- read_csv("./data/clean_bibliography-03.csv", col_names = T)

# quickly verify that everything is in order:
glimpse(initial_dat)
# check for missing data
initial_dat %>% 
  summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble()
            )
# 3. MetaGear screening ---------------------------------------------------
### Create the review excel sheet and initialize format for abstract screening

# bib_unscreened <- effort_distribute(initial_dat, reviewers="drew1", initialize = T, save_split = T, directory = "./output/") #only need to run once, uncomment if needed
# colnames(bib_unscreened)

### Use the built in GUI for metaGear to screen articles -----
# NOTE: YOU HAVE TO SAVE BEFORE QUITTING THE GUI OR YOU'LL LOSE PROGRESS, it will update the effort_*reviewer*.csv file
abstract_screener(file="./output/effort_drew1.csv",
                  aReviewer = "drew",
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
bibscreened<-effort_merge(directory="./output") #this will merge all effort_*reviewer*.csv files together
unique(bibscreened$INCLUDE) #how many categories are there?

# generate a summary table by each decision category
bibscreened %>% 
  group_by(INCLUDE) %>% #group by category
  summarize(count=n()) %>% #summarize based on count of each category
  mutate(percentage = count/nrow(bibscreened)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOtitle" ~ "Excluded based title, no indication of either urban/city or connectivity assessed",
                             INCLUDE == "NOabstr" ~ "Excluded based on abstract, either not urban/city or no metric of connectivity",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "Candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage)) #arrange in descending % order

# What journals are the YES, MAYBE, and REVIEW
bibscreened %>% 
  filter(INCLUDE=="YES" | INCLUDE=="MAYBE") %>% 
  group_by(jour_s) %>% 
  count(jour_s) %>% 
  ggplot(aes(x=n, y = reorder(jour_s,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme(axis.text.y = element_text(size=5)) +  scale_y_discrete(labels = function(x) str_trunc(x, 35))

## How many entries are there for the categories
### For entries in the YES category, #What is the maximum number of articles from 1 journal?
bibscreened %>% filter(INCLUDE=="YES") %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  arrange(desc(n)) 
### For entries in the MAYBE category, #What is the maximum number of articles from 1 journal?
bibscreened %>% filter(INCLUDE=="MAYBE") %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  arrange(desc(n)) 

### How many journals have more than 5 entries (across all decision groups)
bibscreened %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  arrange(desc(n)) %>% 
  filter(n >= 5) %>% n_distinct() 

## Plot all the INCLUDE categories
### pull the list of journals with >5 entries
jlist <- bibscreened %>% filter(INCLUDE!="not vetted") %>% 
  group_by(jour_s) %>% count(jour_s) %>% 
  filter(n >= 5) %>% pull(jour_s)

bibscreened %>% 
  filter(INCLUDE!="not vetted") %>%
  filter(jour_s %in% jlist) %>% #un/comment this line to get ALL the journals, very cluttered with all. 
  group_by(jour_s, INCLUDE) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  ggplot(aes(x= count, y = reorder(jour_s,count), fill = INCLUDE)) + #reorder to descending frequency
    geom_bar(stat="identity", position = "stack", colour="black") +
    labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
    theme_bw()+ #to standardize to a common theme
    theme(axis.text.y = element_text(size = 5)) + #specific modifications to the theme
  scale_y_discrete(labels = function(x) str_trunc(x, 35)) +
  scale_x_continuous(limits = c(0, 120), breaks = seq(0, 120, 5), expand=expand_scale(mult=c(0,0.1))) 

# 5. PRISMA screening results - THIS CAN BE MODIFIED--------------
library(PRISMA2020) #v1.1.1
# Read in the template from the package directory; edit boxtext column to change text
pristemp <- read.csv("./data/PRISMA_template-02.csv") 
prisma <- PRISMA_data(pristemp) #coerced to list, you can also modify this list to generate the flowdiagram. 

# Populate the PRISMA list from dataframe information!
prisma$newstud_text <- "Identification of potential candidate studies for meta-analysis"
prisma$records_screened <- nrow(bibscreened) 
prisma$records_excluded <- nrow(bibscreened %>% filter(INCLUDE == "NOtitle"))
prisma$records_excluded_text <- "Records excluded: \n- Not terrestrial-based\n- Not in urban ecosystem\n- No indication of config/connectivity metric"
  
prisma$dbr_sought_reports <- nrow(bibscreened) - nrow(bibscreened %>% filter(INCLUDE == "NOtitle"))
prisma$dbr_notretrieved_reports <- nrow(bibscreened %>% filter(INCLUDE == "NOabstr"))
prisma$dbr_notretrieved_reports_text <- "Records excluded: \nNot in an urban area or no indication of \nconfiguration or connectivity metric used."
  
prisma$dbr_assessed <- nrow(bibscreened) - nrow(bibscreened %>% filter(INCLUDE == "NOtitle")) - nrow(bibscreened %>% filter(INCLUDE == "NOabstr"))
prisma$dbr_excluded <- bibscreened %>% group_by(INCLUDE) %>% filter(INCLUDE %in% c("YES", "MAYBE", "REVIEW")) %>% count() %>% as.data.frame() %>% rename("reason"=1) 
prisma$new_studies <- bibscreened %>% filter(INCLUDE %in% c("YES","MAYBE")) %>% nrow() 
prisma$new_reports <- bibscreened %>% filter(INCLUDE %in% c("REVIEW")) %>% nrow() 
prisma$included_text <- "Screen full-text"

# create and save the PRISMA plot
prismaplot <- PRISMA_flowdiagram(prisma,
                           fontsize = 12,
                           interactive = F,
                           previous = F,
                           other = F,
                           detail_databases = T,
                           detail_registers = F)
PRISMA_save(prismaplot,
            filename = "./output/PRISMA_summary-04.pdf",
            filetype = NA,
            overwrite = T)

#####TEST ZONE#####
#compare initial dat and bibscreened
bibscreened %>% select(!c(INCLUDE,REVIEWERS,STUDY_ID)) %>% anti_join(initial_dat, by=names(.)) %>% view

x<- anti_join(bibscreened, initial_dat) %>% tibble
y<-initial_dat %>% anti_join(bibscreened) %>% tibble
xy<-anti_join(x,y)
nrow(bibscreened)-nrow(initial_dat)
bibscreened %>% semi_join(initial_dat, by=names(select(.,4:20))) %>% view
