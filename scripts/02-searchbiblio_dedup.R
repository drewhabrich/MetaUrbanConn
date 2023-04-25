## HEADER---------------------------
## Script name: 02-searchbiblio-deduplicaiton
##
## Purpose of script: deduplicate records downloaded using initial search string from search databases
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-21
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
# The .ris files were 'repaired' e.g. had empty info filled automatically using doi management and 'bibfix' shiny app

## 1. Load relevant packages--------
library(tidyverse) #v2.0.0
library(synthesisr) #v0.3.0
# Install CiteSource (currently only on github)
#remotes::install_github("ESHackathon/CiteSource") #uncomment if needed
library(CiteSource)

# 2. Import search results and deduplicate --------------------------------
bibfiles<-list.files("./raw_data/bibfix_results/", full.names=T) #create a list of all the files in the data directory
bibfiles #should be 6 files
import<-read_refs(filename=bibfiles) %>% filter(!is.na(title)) #import as a dataframe, so we can manipulate columns

scop <- read_ref("./raw_data/bibfix_results/REPAIRED-scopus.ris")
wos <- read_refs(filename= c("./raw_data/bibfix_results/REPAIRED-wos1.ris", "./raw_data/bibfix_results/REPAIRED-wos2.ris"))
proq <- read_ref("./raw_data/bibfix_results/Zotero_Proquest_grey.RIS") %>% filter(!is.na(title)) #this one is wonky, has a bunch of empty rows
biox <- read_ref("./raw_data/bibfix_results/Zotero_bioRxiv.ris")
ccorg <- read_ref("./raw_data/bibfix_results/REPAIRED-conservationcorridor.ris")

nrow(scop)+nrow(wos)+nrow(proq)+nrow(biox)+nrow(ccorg)
nrow(import)

## 2.1 CiteSource import with metadata ----
# what files are in the folder?
citation_files <- list.files(path = "./raw_data/bibfix_results/", full.names = TRUE)
citation_files

# identify the filepath to the files to match with source meta-data
file_path <- "./raw_data/bibfix_results/"
# create the metadata tibble to match with the .ris files
metadata_tbl <- tibble::tribble(
  ~files,                             ~cite_sources,     ~cite_labels, 
  "REPAIRED-wos1.ris",                 "wos1st",             "wos1",    
  "REPAIRED-wos2.ris",                 "wos1st",             "wos2",    
  "Zotero_bioRxiv.ris",                "biorxivgrey",         "bioR",    
  "Zotero_Proquest_grey.ris",          "proquestgrey",        "proQ",     
  "REPAIRED-scopus.ris",               "scopus1st",          "scop",    
  "REPAIRED-conservationcorridor.ris", "conscorridor",      "consvcorr"
) %>% 
  # Append the file path to each file name in the 'files' column
  dplyr::mutate(files = paste0(file_path, files))

## Read in citations using metadata table
bibmeta <- read_citations(metadata = metadata_tbl)
## remove redundant columns, and prepare columns for deduplication effort
bibs <- bibmeta %>% select(!c("keywords", "L2", "notes", "date_generated"))
bibs <- bibs %>% mutate(isbn = issn)
bibs <- bibs %>% 
  mutate(source = if_else(is.na(source), journal, source)) %>% #if 'source' is empty, fill with 'journal'
  relocate(source, .after = journal)

bibs <- bibs %>% 
  mutate(source = str_replace_all(source, "&", "and")) %>% 
  mutate(source = str_to_lower(source)) %>% #create modified column
  mutate(source = str_to_title(source)) %>% #coerce to title-case
  mutate(source = str_replace_all(source, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

# How many empty rows are there per column and per source?
bibs %>% group_by(cite_source) %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% as.data.frame(.)

# 2.2. Deduplication with source information ----
# Deduplicate citations 
dedup_results <- dedup_citations(bibs, merge_citations = TRUE, manual_dedup = T)

## Manually review the similarities
View(dedup_results$manual_dedup)

# Get unique citations. This yields a dataframe of all records with duplicates merged, but the originating source information maintained in a new variable called cite_source.
unique_citations <- dedup_results$unique
class(unique_citations)
### Let's check and remove some entries from proquest manually, since there were some issues with the bibliographic format (empty info)
unique_citations <- unique_citations %>% #filter(cite_source == "proquest_i") %>%
  mutate_all(~if_else(str_detect(., "^$"), NA_character_, .)) %>% filter(!is.na(title) == T)

# Count number of unique and non-unique citations from different sources and labels. 
n_unique <- count_unique(unique_citations, include_references = F)
# For each unique citation, determine which sources were present
source_comparison <- compare_sources(unique_citations)
# Visualize the overlap between dataframes and save figures to file
plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
plot_source_overlap_heatmap(source_comparison)
plot_source_overlap_heatmap(source_comparison, plot_type = "percentages")
plot_contributions(n_unique,
                   center = TRUE)

# Bar plot of unique/crossover PER Source with labels
n_unique %>%
  select(cite_source, unique) %>% 
  ggplot(aes(fill=unique, x=cite_source)) + 
  geom_bar(position=position_dodge(width=0.5)) +
  xlab("") + ylab("Number of citations") +
  geom_text(stat="count", aes(label=..count..)) + theme(axis.line = element_line(linetype = "solid"),
                                                        panel.grid.major = element_line(linetype = "blank"),
                                                        axis.text = element_text(size = 11, angle = 90),
                                                        panel.background = element_rect(fill = NA),
                                                        plot.background = element_rect(fill = "white"),
                                                        legend.position = c(0.15, 0.85)) +labs(x = NULL, y = "Number of records")

# What journals/sources are these entries in?
unique_citations %>% 
  group_by(source) %>% 
  count(source) %>% filter(n > 5) %>% 
  ggplot(aes(x=n, y = reorder(source,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme(axis.text.y = element_text(size=6)) +  scale_y_discrete(labels = function(x) str_trunc(x, 35)) + theme(axis.line = element_line(linetype = "solid"),
                                                                                                               panel.grid.major = element_line(linetype = "dashed"),
                                                                                                               panel.background = element_rect(fill = NA))
# How many records per database by year?
class(unique_citations$year)
unique_citations$year <- as.numeric(unique_citations$year)
unique_citations %>% 
  filter(cite_source %in% c("wos1st", "scopus1st", "conscorridor", "biorxivgrey", "proquestgrey")) %>% 
  ggplot(aes(x = year)) +
  geom_histogram(stat="count", binwidth = 1) +
  facet_wrap(~ cite_source) +
  xlim(1980,NA) +
  xlab("Publication year") + ylab("Records found") + theme(axis.line = element_line(linetype = "solid"),
                                                           axis.text.x = element_text(angle = 90),
                                                           panel.background = element_rect(fill = NA))

## SAVE TO FILE
export_ris(unique_citations, filename = "./data/dedup_citesource-02.ris", source_field = "DB", label_field = "N1")

## 2.3 Manual method (THIS WAS BEFORE CITESOURCE WAS RELEASED) ----
## Let's take a look at the data structure, and clean up a bit before deduplicating
nrow(import) #4836 entries
names(import) #some of these columns are useless (and incomplete), some are autogenerated by 'synthesisr'
glimpse(import) #check the structure: dataframe with 21 columns of 'character' type
n_distinct(import$title) #how many distinct articles are there? NOTE: this matches EXACT
sum(is.na(import$title)) #are any rows missing their titles? 

# Let's look at any 'missing data' 
## check for NAs by column
import %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% tibble 
# many have few entries

## 2.1 Deduplicate based on exacty titles
n_distinct(import$title)
titldupe <- deduplicate(import, match_by = "title", method = "exact") #EXACT MATCHES ONLY, no need to review these.

nrow(import)-nrow(titldupe) #How many results were removed this way: 957

## 2.2 Deduplicate based on exact matching doi
n_distinct(titldupe$doi) #how many distinct DOIs are there? 
doidupe <- deduplicate(titldupe, match_by = "doi", method = "exact")

## 2.3 Deduplicate based on text string distance
duplicates_string <- find_duplicates(
  doidupe$title,
  method = "string_osa",
  to_lower = TRUE, #forces all text to lowercase for comparison
  rm_punctuation = TRUE, #removes all punctuation before comparison (useful for papers with subtitles)
  threshold = 15 #the cutoff threshold to decide if the strings are duplicates
)

# we can extract the line numbers from the dataset that are likely duplicated
# this lets us manually review those titles to confirm they are duplicates
manual_check <- review_duplicates(doidupe$title, duplicates_string)
view(manual_check) #manually check to see if the 'matches' are actually the same

# Final cleaning
## Override the entries flagged as duplicate but actually aren't (from manual screening)
final_dup <- synthesisr::override_duplicates(duplicates_string, c(14,228,510,1458,1507,1517,1663)) 
final_res <- extract_unique_references(doidupe, final_dup, type="merge") #find the unique entries from the culled dataframe, using the list of duplicates

# 3. Writing the deduplicated bibliography to file ------------------------
#save the final results list into .ris format
write_refs(final_res,
           format = "ris", #or "bib"
           file = "./data/deduplicated_bib-02"
)

# 4. PRISMA information----
# Download the PRISMA template csv and populate the relevant cells (duplicates, n-source specific)
prismainfo <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
glimpse(prismainfo)
prismainfo[13,"n"] <- nrow(import)-nrow(final_res) #number of duplicates
prismainfo[6, "n"] <- str_c("Web of Science, ", nrow(wos), #find the number of entries per source
                            "; SCOPUS, ", nrow(scop), 
                            "; ProQuest, ", nrow(proq),
                            "; bioRxiv, ", nrow(biox),
                            "; ConservationCorridor, ", nrow(ccorg))

# Write the template to csv to populate later when we generate the PRISMA diagram.
write_csv(prismainfo, file = "./data/PRISMA_template-02.csv")

