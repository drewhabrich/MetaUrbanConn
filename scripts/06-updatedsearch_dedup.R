## HEADER---------------------------
## Script name: Updated search results using CiteSource package
##
## Purpose of script: updated search string results from various databases.
## CiteSource can embed meta-data into the .ris and other bibliographies to
## keep track of during screening (faciliates PRISMA figure production)
##
## Author: Andrew Habrich
##
## Date Created: 2023-03-30
## Date last Modified: 2023-10-25 
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load relevant packages--------
#library(remotes)
# Install CiteSource (currently only on github)
#remotes::install_github("ESHackathon/CiteSource") #uncomment if needed
# Load the necessary libraries
library(tidyverse)
library(CiteSource)

# 2. Import files from multiple sources ----
# what files are in the folder?
citation_files <- list.files(path = "./raw_data/06-upd_search", full.names = TRUE)
citation_files

# identify the filepath to the files to match with source meta-data
file_path <- "./raw_data/06-upd_search/"
# create the metadata tibble to match with the .ris files
metadata_tbl <- tibble::tribble(
  ~files,                     ~cite_sources,     ~cite_labels, 
  "benchmark_articles.ris",   "benchmark",       "benchmark",    
  "manualstem_wos1.ris",      "wos",             "wos1",    
  "manualstem_wos2.ris",      "wos",             "wos2",    
  "manualstem_wos3.ris",      "wos",             "wos3",     
  "manualstem_scopus1.ris",   "scopus",          "scop",    
  "manualstem_proqgrey1.RIS", "proquest",        "proq",    
  "keyreview_articles.ris",   "keyreview",       "revs",    
  "keyreview_bwd.ris",        "keyrevbwd",       "rbwd",  
  "keyreview_fwd.ris",        "keyrevfwd",       "rfwd",    
  "bioRxiv.ris",              "biorxiv",         "biox",
  "beninde2015refs.ris",      "beninderefs",     "beninde",
  "lookingbill2022refs.ris",  "lookingbillrefs", "lookingb",
  "02-dedup_citesource.ris",  "pastsearch",      "dedup"
) %>% 
  # Append the file path to each file name in the 'files' column
  dplyr::mutate(files = paste0(file_path, files))

## Read in citations using metadata table
bibmeta <- read_citations(metadata = metadata_tbl)

# how many empty cells are there per column?
bibmeta %>% group_by(cite_source) %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% as.data.frame(.)
# check the dataframe and remove useless columns
### 2.1 Tidying ----
bibs <- bibmeta 
glimpse(bibs)

## How many unique entries are there in columns, and what are they?
#bibs %>% select(c("source_type","cite_source","cite_label","database","document_type")) %>%
#  distinct() %>% tibble %>% view
bibs %>% summarise(across(everything(), ~ n_distinct(.)))
distinct(bibs, cite_source)

# Merge columns that contain the same data
## Journal names/article source
bibs <- bibs %>% 
          mutate(journal = if_else(is.na(journal), source, journal)) %>% #if 'source' is empty, fill with 'journal'
          relocate(source, .after = journal)

bibs <- bibs %>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(journal = str_to_lower(journal)) %>% #create modified column
  mutate(journal = str_to_title(journal)) %>% #coerce to title-case
  mutate(journal = str_replace_all(journal, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

## How many columns have empty cells NOW?
bibs %>% group_by(cite_source) %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% as.data.frame(.)
### let's inspect the columns missing data
bibs %>% filter(is.na(journal)) %>% tibble()
bibs %>% filter(is.na(author)) %>% tibble()
bibs %>% filter(is.na(abstract)) %>% tibble()

# 3. Deduplication and source information ---------------------------------
# Deduplicate citations (note it wont catch preprints and actual publications!)
dedup_results <- dedup_citations(bibs, manual = T)

## Manually review the similarities
dedup_results$manual_dedup 
# Get unique citations. This yields a dataframe of all records with duplicates merged, but the originating source information maintained in a new variable called cite_source.
unique_citations <- dedup_results$unique
# Count number of unique and non-unique citations from different sources and labels. 
n_unique <- count_unique(unique_citations, include_references = T)

### 3.1 Visual overlap plots ----
# For each unique citation, determine which sources were present
source_comparison <- compare_sources(unique_citations)
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
  geom_text(stat="count", aes(label=..count..), size=3, check_overlap = T) + 
  theme(axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(linetype = "blank"),
    axis.text = element_text(size = 11, angle = 90),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = "white"),
    legend.position = c(0.15, 0.85)) +labs(x = NULL, y = "Number of records")

# What journals/sources are these entries in?
bibs %>% 
  filter(cite_source != "pastsearch") %>% #exclude results from past search
  group_by(source) %>% 
  count(source) %>% filter(n > 10) %>% 
  ggplot(aes(x=n, y = reorder(source,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme(axis.text.y = element_text(size=6)) +  scale_y_discrete(labels = function(x) str_trunc(x, 35)) + theme(axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = NA))

# How many records per database by year?
class(bibs$year)
bibs$year <- as.numeric(bibs$year)
bibs %>% 
  filter(cite_source %in% c("pastsearch", "scopus", "wos", "biorxiv", "proquest")) %>% 
  ggplot(aes(x = year)) +
  geom_histogram(stat="count", binwidth = 1) +
  facet_wrap(~ cite_source) +
  xlim(1980,NA) +
  xlab("Publication year") + ylab("Records found") + theme(axis.line = element_line(linetype = "solid"),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = NA))

## Export for further analysis
export_ris(unique_citations, filename = "./data/06-dedup_updsearch.ris", source_field = "DB", label_field = "N1")

# export_csv(unique_citations, filename = "./data/06-deduplicated_bib.csv", separate="cite_source")
# synthesisr::write_refs(as.data.frame(unique_citations),
#            format = "ris", #or "bib"
#            file = "./data/deduplicated_bib-02-21")

## 4. Save/extract info to PRISMA template -------------------
### Check how many results there were by source
# bibmeta %>% group_by(cite_source) %>% count()
# 
# ### read in template from the package directory
# prismainfo <- read.csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020")) #Check the dataframe to know what rows and columns to replace
# 
# # Studies identified in key reviews (Beninde et al 2015, Lookingbill et al. 2022)
# prismainfo[2, "n"] <- sum(sum(bibmeta$cite_source == "beninderefs") + sum(bibmeta$cite_source == "lookingbillrefs"))
# prismainfo[2, "boxtext"] <- c("Studies from previous reviews on topic")
# prismainfo[3, "n"] <- sum(bibmeta$cite_source == "benchmark")
# prismainfo[3, "boxtext"] <- c("Benchmark inclusion studies identified")
# # Total number of entries
# prismainfo[5, "n"] <- nrow(bibmeta)
# # Find the number of entries per source
# prismainfo[6, "n"] <- str_c("Web of Science, ", sum(bibmeta$cite_source == "wos"), 
#                             "; SCOPUS, ", sum(bibmeta$cite_source == "scopus"), 
#                             "; ProQuest, ", sum(bibmeta$cite_source == "proquest"),
#                             "; bioRxiv, ", sum(bibmeta$cite_source == "biorxiv"),
#                             "; Previous screening effort, ", sum(bibmeta$cite_source == "pastsearch"))
# # Results from fwd + bwd citation chasing
# prismainfo[12, "n"] <- sum(sum(bibmeta$cite_source == "keyrevfwd") + 
#                           sum(bibmeta$cite_source == "keyrevbwd") + 
#                           sum(bibmeta$cite_source == "keyreview"))
# prismainfo[13,"n"] <- nrow(bibmeta)-6510 #number of duplicates
# 
# write_csv(prismainfo, file = "./data/PRISMA_template-02-2.csv")
