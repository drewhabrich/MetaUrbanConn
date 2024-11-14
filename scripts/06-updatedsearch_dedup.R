## HEADER---------------------------
## Script name: 06-updatedsearch_deduplication
##
## Purpose of script: updated search string results from various databases.
## CiteSource can embed meta-data into the .ris and other bibliographies to
## keep track of during screening (faciliates PRISMA figure production)
##
## Author: Andrew Habrich
##
## Date Created: 2023-03-30
## Date last Modified: 2024-11-14 
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load relevant packages--------
# Install CiteSource (currently only on github)
#remotes::install_github("ESHackathon/CiteSource") #uncomment if needed
# Load the necessary libraries
pacman::p_load(CiteSource, tidyverse)

# 2. Import files from multiple sources ----
# what files are in the folder?
citation_files <- list.files(path = "./raw_data/06-upd_search", full.names = TRUE)

# identify the filepath to the files to match with source meta-data
file_path <- "./raw_data/06-upd_search/"
# create the metadata tibble to match with the .ris files
metadata_tbl <- tibble::tribble(
  ~files,                     ~cite_sources,     ~cite_labels, 
  "benchmark_articles.ris",   "Benchmark",       "benchmark",    
  "manualstem_wos1.ris",      "WoS",             "wos1",    
  "manualstem_wos2.ris",      "WoS",             "wos2",    
  "manualstem_wos3.ris",      "WoS",             "wos3",     
  "manualstem_scopus1.ris",   "SCOPUS",          "scop",    
  "manualstem_proqgrey1.RIS", "ProQuest",        "proq",    
  "keyreview_articles.ris",   "Lapoint2015refs",       "revs",    
  "keyreview_bwd.ris",        "KeyBwd Citations",       "rbwd",  
  "keyreview_fwd.ris",        "KeyFwd Citations",       "rfwd",    
  "bioRxiv.ris",              "bioRxiv",         "biox",
  "beninde2015refs.ris",      "Beninde2015refs",     "beninde",
  "lookingbill2022refs.ris",  "Lookingbill2022refs", "lookingb",
  "02-dedup_citesource.ris",  "Initial search",      "dedup"
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

# Merge columns that contain the same data
## Journal names/article source
bibs <- bibs %>% 
          mutate(journal = if_else(is.na(journal), source, journal)) %>% #if 'source' is empty, fill with 'journal'
          relocate(source, .after = journal) %>% #move 'source' to the right of 'journal'
          mutate(journal = str_replace_all(journal, "&", "and")) %>% 
          mutate(journal = str_to_lower(journal)) %>% #create modified column
          mutate(journal = str_to_title(journal)) %>% #coerce to title-case
          mutate(journal = str_replace_all(journal, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

# 3. Deduplication and source information ---------------------------------
# Deduplicate citations (note it wont catch preprints and actual publications!)
dedup_results <- dedup_citations(bibs, manual = T)
## Manually review the similarities
dedup_results$manual_dedup 

# Get unique citations. 
unique_citations <- dedup_results$unique
# Count number of unique and non-unique citations from different sources and labels. 
n_unique <- count_unique(unique_citations, include_references = T)

### 3.1 Visual overlap plots ----
# For each unique citation, determine which sources were present
source_comparison <- compare_sources(unique_citations)
plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
plot_source_overlap_heatmap(source_comparison)
plot_source_overlap_heatmap(source_comparison, plot_type = "percentages")
plot_contributions(n_unique, center = TRUE)

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
unique_citations %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% as.data.frame(.)

uq <- unique_citations %>% 
  mutate(journal = str_replace_all(journal, "&", "and")) %>% 
  mutate(journal = str_to_lower(journal)) %>% #create modified column
  mutate(journal = str_to_title(journal)) %>% #coerce to title-case
  mutate(journal = str_replace_all(journal, "\\b(And|In|Of|The|For)\\b", str_to_lower)) 

uq %>% 
  filter(journal != "") %>% 
  mutate(journal = if_else(journal == "Scopus1st", "Book/Other", journal)) %>% 
  group_by(journal) %>% 
  count(journal) %>% filter(n > 15) %>% 
  ggplot(aes(x=n, y = reorder(journal,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme_bw() +  
  scale_y_discrete(labels = function(x) str_trunc(x, 30)) + 
  theme(axis.text.y = element_text(size = 8),
    axis.line = element_line(linetype = "solid"),
    panel.background = element_rect(fill = NA))

# How many records per database by year?

bibs$year <- as.numeric(bibs$year)
bibs %>% 
  filter(cite_source %in% c("Initial search", "SCOPUS", "WoS", "bioRxiv", "ProQuest")) %>% 
  ggplot(aes(x = year)) +
  geom_histogram(stat="count", binwidth = 1) +
  facet_wrap(~ cite_source) +
  xlim(1980,NA) +
  xlab("Publication year") + ylab("Records found") + theme(axis.line = element_line(linetype = "solid"),
    axis.text.x = element_text(angle = 45),
    panel.background = element_rect(fill = NA))

bibs %>%
  filter(cite_source %in% c("bioRxiv", "Initial search", "ProQuest", "SCOPUS", "WoS")) %>%
  ggplot() +
  aes(x = year) +
  geom_histogram(stat="count", binwidth = 1, fill = "#112446") +
  labs(x = "Publication year",
       y = "Records found",
       title = "Unique records by database") +
  theme_bw() +
  theme(axis.line = element_line(linetype = "solid"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = NA)) + 
  facet_wrap(vars(cite_source))

## Export the ris files for tidying and full-text screening
export_ris(unique_citations, filename = "./data/06-dedup_updsearch.ris", source_field = "DB", label_field = "N1")