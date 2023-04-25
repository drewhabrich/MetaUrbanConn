## HEADER---------------------------
## Script name: Screening of updated search results
##
## Purpose of script: Compiling past screened and new results to be screened
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

# 1. Load relevant packages--------
library(plyr) #plyr needs to be loaded BEFORE tidyverse to prevent issues with dplyr
library(tidyverse) #v2.0.0
library(metagear) #v0.7.0

# 2. Import the cleaned and deduplicated bibliography to be screened -----
initial_dat <- read_csv("./data/clean_bibliography-03-1.csv", col_names = T)
## Quickly verify that everything is in order:
glimpse(initial_dat)
## remove the 'database' column from the dataframe to faciliate merging results from different screening steps
dat <- initial_dat %>% select(!"database")
## initialize the dataframe for screening (uncomment below to initialize)
# bib_unscreened <- effort_distribute(dat, reviewers="drew2", initialize = T, save_split = T, directory = "./output/") #only need to run once, uncomment if needed
# colnames(bib_unscreened)

### Import the previously screened entries and merge them with the new dataframe 
### This minimizes re-screening entries multiple times
prv.screen <- read_csv("./output/effort_drew1.csv", col_names = T)
upd.screen <- read_csv("./output/effort_drew2.csv", col_names = T)

prv.screen <- prv.screen %>% select(!c("end_page","publisher","issue")) #remove useless columns

### merge based on title
merged_df <- upd.screen %>%
  left_join(prv.screen %>% select(title, INCLUDE, doi, url), by = "title", suffix = c("_upd", "_prv")) %>% #select only relevant columns
  mutate(INCLUDE = ifelse(!is.na(INCLUDE_prv), INCLUDE_prv, INCLUDE_upd)) %>% #create new INCLUDE column, if NA in previous, replace with updated 
  relocate(INCLUDE, .after = "REVIEWERS") %>% 
  select(-INCLUDE_upd, -INCLUDE_prv)

### create dataframes for each decision, so we can just screen the unscreened stuff
df_unscreened <- merged_df %>% filter(INCLUDE == "not vetted")
df_rev <- merged_df %>% filter(INCLUDE == "REVIEW")
df_yes <- merged_df %>% filter(INCLUDE == "YES")
df_myb <- merged_df %>% filter(INCLUDE == "MAYBE")

## Write the merged dataframe to csv for screening (RERUNNIG THIS WILL CREATE MORE CSVs, proceed with caution)
#write_csv(merged_df, "./output/upd_screening_effort-04-1.csv", col_names = T) #uncomment if needed, only need to do it once

## Write a csv for each group in the previously screened, for revision later.
# write_csv(df_unscreened, "./output/unscreenedbib-04-1.csv", col_names = T)
# write_csv(df_rev, "./output/prvscreen_review-04-1.csv", col_names = T)
# write_csv(df_yes, "./output/prvscreen_yes-04-1.csv", col_names = T)
# write_csv(df_myb, "./output/prvscreen_maybe-04-1.csv", col_names = T)

# 3. Screen updated entries - with Metagear GUI -----
# NOTE: YOU HAVE TO SAVE BEFORE QUITTING THE GUI OR YOU'LL LOSE PROGRESS, it will update the effort_*reviewer*.csv file
abstract_screener(file="./output/unscreenedbib-04-1.csv",
                  aReviewer = "drew2",
                  unscreenedColumnName = "INCLUDE",
                  unscreenedValue = "not vetted",
                  abstractColumnName = "abstract",
                  titleColumnName = "title",
                  browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=", #query google scholar 
                  fontSize = 14,
                  windowWidth = 85,
                  windowHeight = 20,
                  highlightKeywords = c("urban","city","connectivity","permeability","network","isolat", "isolation",
                                        "species richness", "diversity","corridor", "species", "least cost", "review"),
                  highlightColor = "palegoldenrod",
                  theButtons = c("YES","MAYBE","NOabstr","NOtitle","REVIEW"),
                  keyBindingToButtons = c("q","w","e","r","t")) 

## 3.1. Rescreen prv decisions for verification
### REVIEW PILE
abstract_screener(file="./output/prvscreen_review-04-1.csv",
                  aReviewer = "drew2",
                  unscreenedColumnName = "INCLUDE",
                  unscreenedValue = "REVIEW", #RECHECKING THE 'REVIEW' CATEGORY
                  abstractColumnName = "abstract",
                  titleColumnName = "title",
                  browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=", #query google scholar 
                  fontSize = 14,
                  windowWidth = 85,
                  windowHeight = 20,
                  buttonSize = 12,
                  highlightKeywords = c("review","methodology","connectivity","framework","systematic review","theory",
                                        "theoretic", "urban", "city", "index", "indices","method"),
                  highlightColor = "skyblue",
                  theButtons = c("TopicReview","Methodology","Framework","Theoretical", "Missorted"),
                  keyBindingToButtons = c("q","w","e","r","t")) 

### YES PILE -> ONLY DO THIS IF NECESSARY
# abstract_screener(file="./output/prvscreen_yes-04-1.csv",
#                   aReviewer = "drew2",
#                   unscreenedColumnName = "INCLUDE",
#                   unscreenedValue = "YES", #RECHECKING THE 'REVIEW' CATEGORY
#                   abstractColumnName = "abstract",
#                   titleColumnName = "title",
#                   browserSearch = "https://scholar.google.ca/scholar?hl=en&as_sdt=0%252C5&q=", #query google scholar 
#                   fontSize = 14,
#                   windowWidth = 85,
#                   windowHeight = 20,
#                   buttonSize = 12,
#                   highlightKeywords = c("connectivity","isolation","least-cost","gene","differentiation","circuit",
#                                         "functional", "structural", "configuration", "population", "community"),
#                   highlightColor = "yellow",
#                   theButtons = c("Structural","Functional","Genetic","ObsMovement", "Reevaluate"),
#                   keyBindingToButtons = c("q","w","e","r","t")) 

# 4. Screening effort results ----
## read in the screened results
bibscreened <- read_csv("./output/unscreenedbib-04-1.csv", col_names = T)
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
  arrange(desc(percentage))

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

# 5. Full screening efforts -----------------------------------------------
updscreening <- read_csv("./output/upd_screening_effort-04-1.csv", col_names = T)
updscreening %>% filter(INCLUDE == "not vetted") %>% count("not vetted") #there should be as many rows as bibscreened df

fullscreen <- updscreening %>%
              left_join(bibscreened %>% select(STUDY_ID, INCLUDE), by = "STUDY_ID") %>% # join the two dataframes
              mutate(INCLUDE = if_else(INCLUDE.x == "not vetted", INCLUDE.y, INCLUDE.x)) %>%  #replace 'not vetted' from unscreened entries with the decisions from the upd screening
              relocate(INCLUDE, .after = "REVIEWERS") %>% 
              select(-INCLUDE.x, -INCLUDE.y)

screensummary_table <- fullscreen %>% 
  group_by(INCLUDE) %>% #group by category
  summarize(count=n()) %>% #summarize based on count of each category
  mutate(percentage = count/nrow(fullscreen)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOtitle" ~ "Excluded based title, no indication of either urban/city or connectivity assessed",
                             INCLUDE == "NOabstr" ~ "Excluded based on abstract, either not urban/city or no metric of connectivity",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "Candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage))
write_csv(screensummary_table, file="./output/tables/final_screenres_table-04-1.csv")

## 5.1. Merge citesource ----
# merge results with citesource by screening effort/results
fullscreen <- fullscreen %>% mutate(database = initial_dat$database)
## Write to csv to work with later
write_csv(fullscreen, file = "./data/final_tiab_screening-04-1.csv")

## Visualize results
### Yes, maybe, reviews
fullscreen %>% 
  filter(INCLUDE!="NOabstr" & INCLUDE!="NOtitle") %>%
  group_by(jour_s, INCLUDE) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>% 
  ungroup() %>% 
  ggplot(aes(x= count, y = reorder(jour_s,count), fill = INCLUDE)) + #reorder to descending frequency
  geom_bar(stat="identity", position = "stack", colour="black") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme_bw()+ #to standardize to a common theme
  theme(axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 10, angle = 45)) + #specific modifications to the theme
  scale_y_discrete(labels = function(x) str_trunc(x, 35)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 105), breaks = seq(0, 105, 5))  + theme(axis.text = element_text(hjust = 0.75),
    axis.text.y = element_text(size = 4),
    legend.position = c(0.9, 0.7)) + theme(legend.position = c(0.85, 0.7))

#Just YES
fullscreen %>% 
  filter(INCLUDE=="YES") %>%
  group_by(jour_s) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>% 
  ggplot(aes(x= count, y = reorder(jour_s,count))) + #reorder to descending frequency
  geom_bar(stat="identity", position = "stack", colour="black", fill="goldenrod") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of 'YES' in journals with >1 entry") +
  theme_bw()+ #to standardize to a common theme
  theme(axis.text.y = element_text(size = 5)) + #specific modifications to the theme
  scale_y_discrete(labels = function(x) str_trunc(x, 35)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40), breaks = seq(0, 40, 5)) 

# 6. WIP PRISMA screening results - THIS CAN BE MODIFIED--------------
library(PRISMA2020) #v1.1.1
library(DiagrammeR) #v1.0.9

#prismainfo <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
# Read in the template from the package directory; edit boxtext column to change text
pristemp <- read_csv("./data/PRISMA_template-02-2.csv") 
pristemp <- pristemp %>% select(-1) #remove this column...

## Identify how many entries were removed during tidying; inaccessible
pristemp[15,"n"] <- as.character(as.numeric(pristemp[5,"n"]) - as.numeric(pristemp[13,"n"]) - nrow(initial_dat))
pristemp[15,"boxtext"] <- c("Records removed: inaccessible")

## Coerce to list, you can also modify this list to generate the flowdiagram. 
prisma <- PRISMA_data(pristemp) 

# Populate the PRISMA list from dataframe information! Use NA to remove the text from the diagram
prisma$newstud_text <- "Identification of potential candidate studies for meta-analysis"
prisma$register_results <- NA
prisma$website_results <-  
prisma$organisation_results <- NA
prisma$excluded_automatic <- NA
prisma$other_sought_reports <- NA
prisma$other_notretrieved_reports <- NA
prisma$other_assessed <- NA
prisma$records_screened <- as.numeric(pristemp[5,"n"]) - as.numeric(pristemp[13,"n"]) - as.numeric(pristemp[15,"n"])
prisma$records_screened_text <- c("Records screened on Title")
prisma$records_excluded <- nrow(fullscreen %>% filter(INCLUDE == "NOtitle")) 
prisma$records_excluded_text <- "Exclusion reasons: \n- Not terrestrial-based\n- Not in urban ecosystem\n- No indication of config/connectivity metric"

prisma$dbr_sought_reports <- nrow(fullscreen) - nrow(fullscreen %>% filter(INCLUDE == "NOtitle"))
prisma$dbr_sought_reports_text <- "Records screened on Abstract"
prisma$dbr_notretrieved_reports <- nrow(fullscreen %>% filter(INCLUDE == "NOabstr"))
prisma$dbr_notretrieved_reports_text <- "Exclusion reasons: \nNot in an urban area or no indication of \nconfiguration or connectivity metric used."
prisma$dbr_assessed <- nrow(fullscreen) - nrow(fullscreen %>% filter(INCLUDE == "NOtitle")) - nrow(fullscreen %>% filter(INCLUDE == "NOabstr"))
prisma$dbr_assessed_text <- "Records assessed for eligibility"
prisma$dbr_excluded <- fullscreen %>% filter(INCLUDE %in% c("REVIEW")) %>% count() 
prisma$dbr_excluded_text <- "Records identified as review article"

prisma$new_studies <- fullscreen %>% filter(INCLUDE %in% c("YES","MAYBE")) %>% nrow() 
prisma$new_studies_text <- "Records eligible for full-text screening"
prisma$new_reports <- NA
prisma$new_reports_text <- NA
prisma$total_studies <- fullscreen %>% filter(INCLUDE =="YES") %>% count()
prisma$total_reports <- fullscreen %>% filter(INCLUDE =="MAYBE") %>% count()
prisma$total_studies_text <- "Total studies identified as YES"
prisma$total_reports_text <- "Total studies identified as MAYBE"
prisma$included_text <- "Screen full-text"

# create and save the PRISMA plot
prismaplot <- PRISMA_flowdiagram(prisma,
                                 fontsize = 12,
                                 interactive = F,
                                 previous = T,
                                 other = T,
                                 detail_databases = T,
                                 detail_registers = F)
prismaplot$x$diagram <- gsub("A->19", "2->4", prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("2->A", "", prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("14->15", "", prismaplot$x$diagram)
prismaplot

graph <- prismaplot$x$diagram
class(graph)
grViz(graph)
#print(prismaplot$x$diagram)



# TEST ZONE ----
prismaplot$x$diagram <- gsub("15->16", "", prismaplot$x$diagram);prismaplot$x$diagram <- gsub("15->17", "", prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("17->18", "", prismaplot$x$diagram);prismaplot$x$diagram <- gsub("17->B", "", prismaplot$x$diagram)
prismaplot

DiagrammeR::grViz(prismaplot$x$diagram)
get_node_info(prismaplot)
graph_stmt(prismaplot)

prismaplot$x$diagram <- gsub("pos = \"14.25,1.5!\",", "pos = \"14.25,7.5!\",", prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("arrowhead = normal,\n          arrowtail = none]\n        14->15", "arrowhead = none,\n          arrowtail = none]\n        14->B", prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("arrowhead = none,\n          arrowtail = none]\n        17->B", "arrowhead = normal,\n          arrowtail = none]\n        B->4", prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("edge [\n          color = Black,\n          arrowhead = normal,\n          arrowtail = none,\n          constraint = FALSE\n        ]\n        B->12;", "",prismaplot$x$diagram)
prismaplot$x$diagram <- gsub("node [\n        shape = box,\n        fontname = Helvetica,\n        color = Gainsboro]\n      15 [\n        label = \"Reports sought for retrieval\n(n = NA)\",\n        style = \"filled\",\n        width = 3.5,\n        height = 0.5,\n        pos = \"14.25,5!\",\n        tooltip = \"Reports sought for retrieval (other)\"\n      ]\n      node [\n        shape = box,\n        fontname = Helvetica,\n        color = Gainsboro]\n      16 [\n        label = \"Reports not retrieved\n(n = NA)\",\n        style = \"filled\",\n        width = 3.5,\n        height = 0.5,\n        pos = \"18.25,5!\",\n        tooltip = \"Reports not retrieved (other)\"\n      ]\n      node [\n        shape = box,\n        fontname = Helvetica,\n        color = Gainsboro\n      ]\n      17 [\n        label = \"Reports assessed for eligibility\n(n = NA)\",\n        style = \"filled\",\n        width = 3.5,\n        height = 0.5,\n        pos = \"14.25,3.5!\",\n        tooltip = \"Reports assessed for eligibility (other)\"\n      ]\n      node [\n        shape = box,\n        fontname = Helvetica,\n        color = Gainsboro]\n      18 [\n        label = \"Reports excluded:\nReason1 (n = NA)\nReason2 (n = NA)\nReason3 (n = NA)\",\n        style = \"filled\",\n        width = 3.5,\n        height = 1,\n        pos = \"18.25,3.5!\",\n        tooltip = \"Reports excluded (other)\"\n      ]\n", "",prismaplot$x$diagram)
prismaplot


class(prismaplot)
prisdiagram <- prismaplot$x$diagram
print(prisdiagram)
class(prisdiagram)
# PRISMA_save(prismaplot,
#             filename = "./output/PRISMA_summary-04-1.pdf",
#             filetype = NA,
#             overwrite = T)


class(upd_dat$year)
upd_dat <- upd_dat %>% mutate(year=as.numeric(year))
rmvdrows<-anti_join(upd_dat, initial_dat, by="title")
