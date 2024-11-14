## HEADER---------------------------
## Script name: 01-litsearch keyword optimization
##
## Purpose of script: Take an initial set of keywords on a topic and download 'naive' search results from a search database 
##
## Author: Andrew Habrich
##
## Date Created: 2023-3-1
## Date last modified: 2024-11-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
# get current date

## 1. Load relevant packages--------
rm(list = ls())

## 1. Load relevant packages ###################################################
pacman::p_load(tidyverse, ggpubr, RColorBrewer, easystats,
               litsearchr, igraph, ggraph, remotes)
install_github("elizagrames/litsearchr", ref="main")
sessionInfo()

# 2. Importing results from initial search string ----------------------------
# Downloads all files in folder if no specific file is referenced
initial_results <- import_results(directory = "raw_data/01-naive_search") 
#check the data structure/format
colnames(initial_results)

# Generating potential search terms
## First we need to include generic words to exclude from generation of new keywords
## DEPENDING ON WHAT YOU ARE SEARCHING FOR THE STOPWORDS MAY NEED TO BE MODIFIED
englishstopwords <- get_stopwords("English")
ecologystopwords <- read_lines("./raw_data/ecologystopwords.txt")
stopwords <- c(englishstopwords,ecologystopwords) 

### From keywords
# Keywords will need pruning; compare against the titles/abstracts and sort into groups later.
initial_results$keywords %>% is.na() %>% sum() #351 results missing keywords
keywords <- extract_terms(keywords = clean_keywords(initial_results$keywords), 
              method = "tagged", #keywords used by authors in the article
              min_freq = 25, #minimum # of times it needs to be used
              min_n = 1) #shortest tag can be 1 word 
 
## From titles 
titleterms <- extract_terms(initial_results$title,
              method = "fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
              min_freq = 10, # >10 time occurrence
              min_n = 2, #at least 2 word tags
              stopwords = stopwords) 
titleterms

## Combine keywords and titleterms into one dataframe, using only the unique terms
combinedterms <- unique(c(keywords, titleterms)) 

## Remove unrelated and redundant terms (like area or landscape)
# Define the set of terms to remove (generic 1 word terms that don't mean much alone)
remove_terms <- c("area","areas","system","systems","model","models","impact","impacts","accessibility","big data","atlantic forest","australia","availability","bacteria","basin","benefit","benefits","brazil"  ,"california","challenges","children","china","citizenship","classification","computer-program","consequences","conservation","conservation planning","consumption","contamination","cover","covid-19","determinants","disease","dna","drivers","dengue","europe","epidemiology","ecosystem","ecosystems","establishment","gis","gentrification","habitat","history","landslide","landslides","nitrogen","model", "models","index","indexes", "land", "region", "national", "time", "space", "range", "forest", "water", "river", "size", "design", "future", "state", "field", "health", "people", "person", "planning", "place", "performance", "management", "risk", "service", "services", "software") 

terms <- combinedterms[!(combinedterms %in% c(remove_terms))]

# 3. Network analysis and keyword pruning ------------------------------------
# Bind the title and abstract text together to visualize the term network
fulldoc <- paste(initial_results[, "title"], initial_results[, "abstract"]) 
docfmatrix <- create_dfm(elements = fulldoc, features = terms) #create the document matrix
graph <- create_network(docfmatrix, min_studies = 3, min_occ = 3) #create a network of terms used by the records

# Prune according to keyword strength in network
strengths <- strength(graph)
term_strengths <- data.frame(term = names(strengths), strength =strengths, row.names=NULL) %>%
  mutate(rank = rank(strength, ties.method="min")) %>%
  arrange(desc(strength))
term_strengths ##Lower rank = less important

#visualize cutoff figure to decide
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)
cutoff_cum <- find_cutoff(graph, method="cumulative", percent=0.8) #retain 80% network strength

cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")

get_keywords(reduce_graph(graph, cutoff_cum)) #a list of the 'strongest' keywords
hist(igraph::strength(graph), 100, main="Histogram of term-node strengths", xlab="Term-node strength")

## Grouping keywords to write a new 'refined' search string; NOTE: exclude fish/non-terrestrial taxa
grouped_terms <-list(
  urban=c("urban","urban areas","urban landscapes","city","cities","towns"),
  biodiversity=c("biodiversity","wildlife","vegetation","plants","trees","birds","avian","arthropods","insects","mammals","herptiles","reptiles", "amphibians"),
  
  connectivity=c("connectivity","landscape connectivity","functional connectivity","structural connectivity","habitat connectivity","ecological connectivity","habitat networks","ecological networks","corridors","habitat corridors","ecological corridors","least-cost","least cost","circuit theory","circuit-theory","landscape resistance","landscape permeability"),
  
  outcomes=c("occupancy","occurrence","abundance", "species richness", "diversity", "species responses", "genetic diversity","gene flow", "species distribution", "species density", "species composition", "species assemblages", "community composition", "community assemblages", "population persistance", "population responses", "distribution patterns", "spatial distribution","movement","dispersal"))

# 4. Writing boolean searches for databases ----------------------------------
# from the newly grouped terms
write_search(
  grouped_terms,
  languages = "English",
  exactphrase = T,
  stemming = T,
  closure = "full",
  directory= "./data/01-initial_string", #saved to output folder
  writesearch = T
)