## HEADER---------------------------
## Script name: 01-1-litsearch_reoptimization
##
## Purpose of script: Reoptimize keywords using litsearchr, using the screened results,
## benchmarked articles, and fwd/bwd citationchasing from key reviews
##
## Author: Andrew Habrich
##
## Date Created: 2023-03-29
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca

# 1. Literature search preparation -------------------------------------------
#remotes::install_github("rmetaverse/metaverse") #this installs a whole set of meta-analysis related R packages, https://rmetaverse.github.io/
library(tidyverse) #v2.0.0
library(synthesisr) #v0.3.0
library(litsearchr) #v1.0.0
library(igraph) #v1.3.5
library(ggraph) #v2.1.0

## First we need to include generic words to exclude from generation of new keywords
englishstopwords <- litsearchr::get_stopwords("English")
ecologystopwords <- read_lines("./raw_data/ecologystopwords.txt")
stopwords <- unique(append(englishstopwords,ecologystopwords))

# 2. Reinforming search strategy ---- 
## with Ti/Ab screened results from the previous search string.
## Read in the csv of screened results
bibscreened<-read_csv("./output/effort_drew.csv")
glimpse(bibscreened)
## Randomly select 100 entries in the "YES" and "MAYBE" with abstract and DOI
set.seed(888) #set seed for reproducibility 
bs_subset <- bibscreened %>% 
  filter(INCLUDE == "YES" | INCLUDE == "MAYBE") %>% 
  filter(!is.na(title) & !is.na(abstract)) %>%  
  sample_n(100)

# 3. read in the collected datasets ----
## This includes: 19 benchmark articles, a random subset of 100 screened articles, 
## 6 key connectivity reviews, and the forward and backward citations from those reviews.
res <- import_results(directory="raw_data/search_optimization")
glimpse(res)

## 3.1 subset of screened articles terms ----
bs_titleterms <- extract_terms(bs_subset$title,
                          method = "fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
                          ngrams = T, 
                          min_freq = 2, # minimum term occurrence for inclusion
                          min_n = 2, # minimum number of words per term
                          stopwords = englishstopwords)
bs_titleterms
bs_abstrterms <-extract_terms(bs_subset$abstract,
                          method = "fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
                          ngrams = T, 
                          min_freq = 3, # minimum term occurrence for inclusion
                          min_n = 2, # minimum number of words per term
                          stopwords = stopwords)
bs_abstrterms
## 3.2 benchmark and review articles terms ----
### We can be more restrictive on how frequently the terms need to come up for inclusion, since there more entries here.
res_titleterms<-extract_terms(res$title,
                          method = "fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
                          ngrams = T, 
                          min_freq = 5, # minimum term occurrence for inclusion
                          min_n = 2, # minimum number of words per term
                          stopwords = stopwords)
res_titleterms
res_abstrterms<-extract_terms(res$abstract,
                          method = "fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
                          ngrams = T, 
                          min_freq = 10, # minimum term occurrence for inclusion
                          min_n = 2, # minimum number of words per term
                          stopwords = stopwords)
res_abstrterms
## clean and combine terms list
### combine
terms <- unique(c(res_titleterms, res_abstrterms, bs_titleterms, bs_abstrterms))
terms # there are some directional or geographic terms, which could bias the search

### put together a set of terms to remove
remove_terms <- c("atlantic forest", "complex landscapes", "european cities", "great plains", "hydrological connectivity", 
                  "nature conservation", "rapid urbanization", "security pattern","spontaneous plant","",
                  "ecological security pattern","quantifying landscape","alien species","cities worldwide","findings suggest","important factor","increasingly important","negatively affected",
                  "recent years","scientific literature","significant differences","water bodies","rights reserved","geographic information","major threat","negative consequences",
                  "paper presents","poorly understood","predictive power","provide insights","stepping stones","business media","springer science",
                  "major","identified","influence","increasing","generally","function","including","important","methods"
                  ) 
### remove from the final set of terms to generate the network from
terms <- terms[!terms %in% c(remove_terms)]

# 4. Network analysis and keyword pruning ------
fulldoc <- paste(res[, "title"], res[, "abstract"]) #bind the title and abstract text
docfmatrix <- create_dfm(elements=fulldoc, features=terms) #create the document matrix
bib_network<- create_network(docfmatrix, min_studies=5, min_occ = 5) #create a network of terms used by the records

## visualize the network, before culling 
ggraph(bib_network, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), hjust="outward", check_overlap=TRUE)

## Prune according to 'keyword strength' in network
## Lower rank = less important
kw_str <- strength(bib_network)
term_str <- data.frame(term=names(kw_str), strength = kw_str, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(desc(strength)) #arranges in ascending order by default
tibble(term_str)

## visualize cutoff figure to decide
ggplot(term_str, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_str, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE) + 
  # plot the cutoff value, retaining 75% of the network strength
  geom_hline(yintercept = find_cutoff(bib_network, method="cumulative", percent=0.75), 
             linetype="dashed") +
  theme_bw()
# What is the range of 'strengths' of the various terms?
hist(igraph::strength(bib_network), 100, main="Histogram of node strengths", xlab="Node strength")

## generate a list of the keywords that contribute the top 50% of the network strength
get_keywords(reduce_graph(bib_network, 
                          find_cutoff(bib_network, method="cumulative", percent=0.75))) 
## what if we used the changepoint as cutoffs?
ggplot(term_str, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_str, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE) + 
  # plot the cutoff value, of the 3 strength changepoints
  geom_hline(yintercept = find_cutoff(bib_network, method="changepoint", knot_num=3), 
             linetype="dashed") +
  theme_bw()

###' The first cutoff point is similar to the 75% retention, so let's try that.
selected_terms <- get_keywords(reduce_graph(bib_network, 
                          find_cutoff(bib_network, method="cumulative", percent=0.75))) 
write.csv(selected_terms, "./output/search_terms-01-1.csv")

## 4.1 Grouping keywords according to a PICO framework ----
### These terms will be used to write a new 'refined' search string 
### NOTE excluding fish/non-terrestrial
grouped_terms <- read.csv("./output/grouped_terms-01-1.csv")
glimpse(grouped_terms)

### Extract the terms for each group from the csv into a list
grouped_terms <- list(
  urban = grouped[grepl("urban", grouped$group), "x"],
  biodiversity = grouped[grepl("biodiversity", grouped$group), "x"],
  outcome = grouped[grepl("outcome", grouped$group), "x"],
  connectivity = grouped[grepl("connectivity", grouped$group), "x"]
)

## make a list of the previous terms used in the initial search
old_terms <- list(
  urban = c("urban", "urban areas", "city", "cities", "town"),
  biodiversity = c("biodiversity", "wildlife", "plants", "trees", "birds", "avian", "arthropods", "insects", "mammals", "herptiles", "reptiles", "amphibians"),
  outcome = c("occupancy", "occurrence", "abundance", "species richness", "diversity", "species responses", "genetic diversity", "gene flow", "species distributions", "species density", "species compositions", "species assemblages", "community composition", "community assemblages", "population persistance", "population responses", "distribution pattern", "spatial distribution", "movement", "dispersal"),
  connectivity = c("connectivity", "landscape connectivity", "functional connectivity", "structural connectivity", "habitat connectivity", "ecological connectivity", "habitat network", "ecological network", "corridor", "habitat corridor", "ecological corridor", "least-cost", "least cost", "circuit theory", "circuit-theory", "landscape resistance", "landscape permeability", "graph theory", "graph-theory", "isolation")
)
## merge the lists
src_terms <- Map(c, old_terms, grouped_terms)

# 5. Writing boolean searches for databases ----------------------------------
# from the newly grouped terms
# write_search(
#   src_terms,
#   languages = "English", 
#   exactphrase = T,
#   stemming = T,
#   closure = "none",
#   directory= "./output/upd_exactfull_01-1", 
#   writesearch = T
# )

# clean list of terms
cleanterms <- list(
  urban = c("urban", "urban areas", "city", "cities", "town", "urban environment","urban forest","urban matrix","urban greenspace","urban green","urban landscape","green infrastructure"),
  biodiversity = c("biodiversity", "wildlife", "plants", "trees", "birds", "avian", "arthropods", "insects", "mammals", "herptiles", "reptiles", "amphibians"),
  outcome = c("occupancy",	"occurrence",	"abundance",	"species richness",	"species responses",	"species density",	"species composition",	"species assemblages",	"species diversity",	"community composition",	"community assemblages",	"population responses",	"population persistence",	"population density",	"population differentiation",	"animal movement",	"dispersal distance",	"dispersal rate",	"genetic structure",	"gene flow",	"genetic diversity",	"genetic variation",	"distribution pattern",	"spatial distribution",	"diversity"),
  connectivity = c("connectivity",	"patch connectivity",	"landscape connectivity",	"functional connectivity",	"structural connectivity",	"habitat connectivity",	"ecological connectivity",	"habitat network",	"ecological network",	"habitat corridor",	"ecological corridor",	"least cost",	"least-cost",	"graph theory",	"graph-theory",	"circuit theory",	"circuit-theory",	"landscape resistance",	"landscape permeability",	"resistance surface",	"isolation",	"patch isolation",	"habitat isolation")
)

stemmedterms <- list(
  urban = c("urban", "urban area*", "city", "cities", "town*", "urban environment*","urban forest*","urban matrix","urban greenspace*","urban green","urban landscape*","green infrastructure*"),
  biodiversity = c("biodiversity", "wildli*", "plant*", "tree*", "bird*", "avian", "arthropod*", "insect*", "mammal*", "herptile*", "reptile*", "amphibian*"),
  outcome = c("occupan*",	"occurr*",	"abundan*",	"species richness",	"species response*",	"species densit*",	"species composition*",	"species assemblage*",	"species diversity",	"community composition*",	"community assemblage*",	"population response*",	"population persist*",	"population densit*",	"population differentiation",	"animal movement*",	"dispersal distance*",	"dispersal rate*",	"genetic structure",	"gene flow",	"genetic diversity",	"genetic variation",	"distribution pattern*",	"spatial distribution*",	"diversity"),
  connectivity = c("connectivity",	"patch connectivity",	"landscape connectivity",	"functional connectivity",	"structural connectivity",	"habitat connectivity",	"ecological connectivity",	"habitat network*",	"ecological network*",	"habitat corridor*",	"ecological corridor*",	"least cost",	"least-cost",	"graph theor*",	"graph-theor*",	"circuit theory",	"circuit-theory",	"landscape resistance*",	"landscape permeabilit*",	"resistance surface*",	"isolation",	"patch isolation",	"habitat isolation")
)

# write search strings
litsearchr::write_search(
  cleanterms,
  languages = "English", 
  exactphrase = T,
  stemming = T,
  closure = "none",
  directory= "./output/upd_exactfull_string01-1", 
  writesearch = F,
  verbose = T
)

litsearchr::write_search(
  stemmedterms,
  languages = "English", 
  exactphrase = T,
  stemming = F,
  closure = "full",
  directory= "./output/manualstem_exactfull_string01-1", 
  writesearch = T,
  verbose = T
)
