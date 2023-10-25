## HEADER---------------------------
## Script name: 02-searchbiblio-deduplicaiton
##
## Purpose of script: deduplicate records downloaded using initial search string from search databases
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-21
## Date last Modified: 2023-10-24 
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
# The .ris files were 'repaired' (empty info filled automatically using DOIs with 'bibfix' shiny app)

## 1. Load relevant packages--------
library(tidyverse) #v2.0.0
library(synthesisr) #v0.3.0
# Install CiteSource (currently only on github)
# remotes::install_github("ESHackathon/CiteSource") #uncomment if needed
library(CiteSource) #v0.0.1

## 2. Import search results and deduplicate with CiteSource (+metadata) ----
# what files are in the folder?
citation_files <- list.files(path = "./raw_data/02-bibfix_results/", full.names = TRUE)
citation_files

# identify the filepath to the files to match with source meta-data
file_path <- "./raw_data/02-bibfix_results/"
# create the metadata tibble to match with the .ris files
metadata_tbl <- tibble::tribble(
  ~files,                             ~cite_sources,     ~cite_labels, 
  "REPAIRED-wos1.ris",                 "wos1st",             "wos1",    
  "REPAIRED-wos2.ris",                 "wos1st",             "wos1",    
  "Zotero_bioRxiv.ris",                "biorxivgrey",         "bioR",    
  "Zotero_Proquest_grey.ris",          "proquestgrey",        "proQ",     
  "REPAIRED-scopus.ris",               "scopus1st",          "scop",    
  "REPAIRED-conservationcorridor.ris", "conscorridor",      "consvcorr"
) %>% 
  # Append the file path to each file name in the 'files' column
  dplyr::mutate(files = paste0(file_path, files))

## Read in citations using metadata table
bibmeta <- read_citations(metadata = metadata_tbl)
glimpse(bibmeta)

## how many NAs are there per column
bibmeta %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

## remove redundant columns, and prepare columns for deduplication effort
bibs <- bibmeta %>% 
  mutate(isbn = issn) %>% 
  mutate(litsource = if_else(is.na(source), journal, source)) %>% #if 'source' is empty, fill with 'journal'
  relocate(litsource, .after = journal)

bibs %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

## fix journal/source names to be consistently in title-case
bibs <- bibs %>% 
  mutate(litsource = str_replace_all(litsource, "&", "and")) %>% 
  mutate(litsource = str_to_lower(litsource)) %>% #create modified column
  mutate(litsource = str_to_title(litsource)) %>% #coerce to title-case
  mutate(litsource = str_replace_all(litsource, "\\b(And|In|Of|The|For)\\b", str_to_lower)) #conjunctions to lowercase

# How many empty rows are there per column and per source?
bibs %>% group_by(cite_source) %>% summarise(across(everything(), ~ sum(is.na(.)))) %>% as.data.frame(.)

### 2.1. Deduplication with source information ----
# Deduplicate citations 
bib_j <- bibs %>% mutate(journal = litsource) %>% select(!c("source","litsource"))
dedup_results <- dedup_citations(bib_j, manual = T) #you can manually review duplicates if unsure
## Manually review the similarities
View(dedup_results$manual_dedup)

# Get unique citations. This yields a dataframe of all records with duplicates merged, but the originating source information maintained in a new variable called cite_source.
unique_citations <- dedup_results$unique
unique_citations %>% summarise(across(everything(), ~ sum(any(is.na(.)))) %>% as_tibble())

### Let's check and remove some entries from proquest manually, since there were some issues with the bibliographic format (empty info), particularly for proquest
unique_citations <- unique_citations %>% 
  mutate_all(~if_else(str_detect(., "^$"), NA_character_, .)) %>% filter(!is.na(title) == T)

unique_citations %>% summarise(across(everything(), ~ sum(is.na(.))) %>% as_tibble())

# Count number of unique and non-unique citations from different sources and labels. 
n_unique <- count_unique(unique_citations, include_references = F)
# For each unique citation, determine which sources were present
source_comparison <- compare_sources(unique_citations)

### 2.2. Visualize the overlap between dataframes and save figures to file ----
plot_source_overlap_upset(source_comparison, decreasing = c(TRUE, TRUE))
plot_source_overlap_heatmap(source_comparison)
plot_source_overlap_heatmap(source_comparison, plot_type = "percentages")
plot_contributions(n_unique, center = TRUE)

# Bar plot of unique/crossover PER Source with labels
n_unique %>%
  select(cite_source, unique) %>%
  ggplot(aes(fill = unique, x = cite_source)) +
  geom_bar(position = position_dodge(width = 0.5)) +
  xlab("") + ylab("Number of citations") +
  geom_text(stat = "count", aes(label = ..count..)) +
  theme(
    axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(linetype = "blank"),
    axis.text = element_text(size = 11, angle = 90),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = "white"),
    legend.position = c(0.15, 0.85)
  ) + labs(x = NULL, y = "Number of records")

# What journals/sources are these entries in?
unique_citations %>% 
  group_by(source) %>% 
  count(source) %>% filter(n > 5) %>% 
  ggplot(aes(x=n, y = reorder(source,n))) + #reorder to descending frequency
  geom_bar(stat="identity", fill = "steelblue", color = "white") +
  labs(x = "Frequency", y = "Journal", title = "Frequency of entries by Journal") +
  theme(axis.text.y = element_text(size=6)) +  scale_y_discrete(labels = function(x) str_trunc(x, 35)) + 
  theme(
    axis.line = element_line(linetype = "solid"),
    panel.grid.major = element_line(linetype = "dashed"),
    panel.background = element_rect(fill = NA)
  )

# How many records per database by year?
unique_citations$year <- as.numeric(unique_citations$year) #coerce to numeric for plotting
unique_citations %>%
  filter(cite_source %in% c("wos1st", "scopus1st", "conscorridor", "biorxivgrey", "proquestgrey")) %>%
  ggplot(aes(x = year)) +
  geom_histogram(stat = "count", binwidth = 1) +
  facet_wrap( ~ cite_source) +
  xlim(1980, NA) +
  xlab("Publication year") + ylab("Records found") + theme(
    axis.line = element_line(linetype = "solid"),
    axis.text.x = element_text(angle = 90),
    panel.background = element_rect(fill = NA))

## SAVE TO FILE
export_ris(unique_citations, filename = "./data/02-dedup_citesource.ris", source_field = "DB", label_field = "N1")

# 3. WIP-PRISMA information---- 
# Download the PRISMA template csv and populate the relevant cells (duplicates, n-source specific)
# prismainfo <- read_csv(system.file("extdata", "PRISMA.csv", package = "PRISMA2020"))
# glimpse(prismainfo)
# prismainfo[13,"n"] <- nrow(import)-nrow(final_res) #number of duplicates
# prismainfo[6, "n"] <- str_c("Web of Science, ", nrow(wos), #find the number of entries per source
#                             "; SCOPUS, ", nrow(scop), 
#                             "; ProQuest, ", nrow(proq),
#                             "; bioRxiv, ", nrow(biox),
#                             "; ConservationCorridor, ", nrow(ccorg))
# 
# # Write the template to csv to populate later when we generate the PRISMA diagram.
# write_csv(prismainfo, file = "./data/PRISMA_template-02.csv")

