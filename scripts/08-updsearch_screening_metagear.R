## HEADER---------------------------
## Script name: 08-updsearch_screening_metagear
##
## Purpose of script: Compiling past screened and new results to be screened from
##                    the updated search. Screening with 'metagear' package.
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-05
## Date last Modified: 2024-11-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list=ls())
   
# 1. Load relevant packages--------
pacman::p_load(plyr, tidyverse, synthesisr, metagear)

# 2. Import the cleaned and deduplicated bibliography to be screened -----
initial_dat <- read_csv("./data/07-clean_upd_bibliography.csv", col_names = T)

# initialize the dataframe for screening (uncomment below to initialize); only need to run once
# bib_unscreened <- effort_distribute(initial_dat, reviewers="drew_08", initialize = T, save_split = T, directory = "./data/") 

### Import the previously screened entries and merge them with the new dataframe 
### This minimizes re-screening entries multiple times
prv.screen <- read_csv("./data/effort_drew_04.csv", col_names = T)
upd.screen <- read_csv("./data/effort_drew_08.csv", col_names = T)

### merge based on label
merged_df <- upd.screen %>%
  left_join(prv.screen %>% select(title, INCLUDE, doi, url, label), 
            by = "label", suffix = c("_upd", "_prv")) %>% #select only relevant columns
  #create new INCLUDE column, if NA in previous, replace with updated 
  mutate(INCLUDE = ifelse(!is.na(INCLUDE_prv), INCLUDE_prv, INCLUDE_upd)) %>% 
  relocate(INCLUDE, .after = "REVIEWERS") %>% 
  select(-INCLUDE_upd, -INCLUDE_prv)

### create dataframes for each decision, so we can just screen the unscreened stuff
df_unscreened <- merged_df %>% filter(INCLUDE == "not vetted")
df_rev <- merged_df %>% filter(INCLUDE == "REVIEW")
df_yes <- merged_df %>% filter(INCLUDE == "YES")
df_myb <- merged_df %>% filter(INCLUDE == "MAYBE")

## Write the merged dataframe to csv for screening (RERUNNIG THIS WILL CREATE MORE CSVs, proceed with caution)
#write_csv(merged_df, "./data/08-upd_screening_effort.csv", col_names = T) #uncomment if needed, only need to do it once

## Write a csv for each group in the previously screened, for revision later.
# write_csv(df_unscreened, "./data/08-unscreenedbib.csv", col_names = T)
# write_csv(df_rev, "./data/08-prvscreen_review.csv", col_names = T)
# write_csv(df_yes, "./data/08-prvscreen_yes.csv", col_names = T)
# write_csv(df_myb, "./data/08-prvscreen_maybe.csv", col_names = T)

# 3. Screen updated entries - with Metagear GUI -----
# NOTE: YOU HAVE TO SAVE BEFORE QUITTING THE GUI OR YOU'LL LOSE PROGRESS, it will update the effort_*reviewer*.csv file
abstract_screener(file="./data/08-unscreenedbib.csv",
                  aReviewer = "drew_08",
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

#########################
# 4. Screening effort results -----------------------------------------------
## read in the screened results
bibscreened <- read_csv("./data/08-unscreenedbib.csv", col_names = T)
bibscreened %>% 
  group_by(INCLUDE) %>% #group by category
  tally() %>% #summarize based on count of each category
  mutate(percentage = n/nrow(bibscreened)*100, #estimate the percentage in each category
         summary = case_when(INCLUDE == "NOtitle" ~ "Excluded based title, no indication of either urban/city or connectivity assessed",
                             INCLUDE == "NOabstr" ~ "Excluded based on abstract, either not urban/city or no metric of connectivity",
                             INCLUDE == "MAYBE" ~ "Unclear eligibility, needs full text screening",
                             INCLUDE == "YES" ~ "Candidate studies identified",
                             INCLUDE == "REVIEW" ~ "Review/Methodology/Framework articles",
                             TRUE ~ "IN PROGRESS, not vetted yet")) %>% 
  arrange(desc(percentage))

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
updscreening <- read_csv("./data/08-upd_screening_effort.csv", col_names = T)

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
write_csv(screensummary_table, file="./output/tables/08-final_screenres_table.csv")

## 5.1. Merge citesource ----
# merge results with citesource by screening effort/results
fullscreen <- fullscreen %>% mutate(database = initial_dat$database)
## Write to csv to work with later
write_csv(fullscreen, file = "./data/08-final_tiab_screening.csv")