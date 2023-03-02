# Literature search preparation -------------------------------------------
#library(remotes) #install.packages('remotes') #lets you download R packages outside of CRAN. Use as needed
#remotes::install_github("rmetaverse/metaverse") #this installs a whole set of meta-analysis related R packages, https://rmetaverse.github.io/
#Relevant packages; litsearchr, revtools, synthesisr, metaDigitise, robvis. Load as needed.
library(tidyverse) #v1.3.2
library(litsearchr) #v1.0.0
library(igraph) #v1.3.5
library(ggraph) #v2.1.0

# 1. Importing results from initial search string ----------------------------
initial_results<-import_results(directory="raw_data/naive_search") #Downloads all files in folder if no specific file is referenced
#check the data structure/format
str(initial_results)
colnames(initial_results)

# Generating potential search terms
## First we need to include generic words to exclude from generation of new keywords
englishstopwords <- get_stopwords("English")
sciencestopwords <- read_lines("./raw_data/sciencestopwords.txt")
ecologystopwords <- read_lines("./raw_data/ecologystopwords.txt")
stopwords <- c(englishstopwords,sciencestopwords,ecologystopwords) 

### From keywords
naive_results$keywords %>% is.na() %>% sum() #351 results missing keywords
keywords<-extract_terms(keywords = clean_keywords(initial_results$keywords), 
              method = "tagged", #keywords used by authors in the article
              min_freq = 25, #minimum # of times it needs to be used
              min_n = 1) #shortest tag can be 1 word 
keywords #needs some pruning, we can compare against the titles/abstracts and sort into groups later.

## From titles 
titleterms<-extract_terms(initial_results$title,
              method = "fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
              min_freq = 10, # >10 time occurrence
              min_n = 2, #at least 2 word tags
              stopwords = stopwords) 
titleterms
## Combine keywords and titleterms into one dataframe, using only the unique terms
combinedterms<-unique(c(keywords, titleterms)) #number of unique terms, ranging from 1-4 words
combinedterms
# lots of junk terms still included, let's get rid of the most redundant ones included (like area or landscape)
# define the set of terms to remove (generic 1 word terms that don't mean much alone)
remove_terms <- c("area","areas","system","systems","model","models","impact","impacts","accessibility","big data","atlantic forest","australia","availability","bacteria","basin","benefit","benefits","brazil"  ,"california","challenges","children","china","citizenship","classification","computer-program","consequences","conservation","conservation planning","consumption","contamination","cover","covid-19","determinants","disease","dna","drivers","dengue","europe","epidemiology","ecosystem","ecosystems","establishment","gis","gentrification","habitat","history","landslide","landslides","nitrogen","model", "models","index","indexes", "land", "region", "national", "time", "space", "range", "forest", "water", "river", "size", "design", "future", "state", "field", "health", "people", "person", "planning", "place", "performance", "management", "risk", "service", "services", "software") 
terms<-combinedterms[!(combinedterms %in% c(remove_terms))]

# 2. Network analysis and keyword pruning ------------------------------------
fulldoc <- paste(naive_results[, "title"], naive_results[, "abstract"]) #bind the title and abstract text
docfmatrix <- create_dfm(elements=fulldoc, features=terms) #create the document matrix
graph <- create_network(docfmatrix, min_studies=3, min_occ = 3) #create a network of terms used by the records

#Prune according to keyword strength in network
strengths <- strength(graph)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) %>%
  mutate(rank=rank(strength, ties.method="min")) %>%
  arrange(desc(strength)) ->
  term_strengths
term_strengths ##Lower rank = less important

#visualize cutoff figure to decide
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)
cutoff_fig

cutoff_cum <- find_cutoff(graph, method="cumulative", percent=0.8) #retaining 80% of the network strength
cutoff_cum

cutoff_fig +
  geom_hline(yintercept=cutoff_cum, linetype="dashed")


get_keywords(reduce_graph(graph, cutoff_cum)) #a list of the 'strongest' keywords

hist(igraph::strength(g), 100, main="Histogram of node strengths", xlab="Node strength")

#Changepoints (multiple keyword cutoffs)
#cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)
#cutoff_change
#cutoff_fig +
#  geom_hline(yintercept=cutoff_change, linetype="dashed")

#g_redux <- reduce_graph(g, cutoff_change[1])

#selected_terms <- get_keywords(g_redux)
#selected_terms

## Grouping keywords (manually) to write a new 'refined' search string ##NOTE excluding fish/non-terrestrial
grouped_terms1 <-list(
  urban=c("urban","urban area*","urban landscape*","city","cities","town*"),
  biodiversity=c("biodiversity","wildlife","vegetation","plant*","tree*","bird*","avian","arthropod*","insect*","mammal*","herptile*","reptile*", "amphibian*"),
  connectivity=c("connectivity","landscape connectivity","functional connectivity","structural connectivity","habitat connectivity","ecological connectivity","habitat network*","ecological network*","corridor*","habitat corridor*","ecological corridor*","least-cost","least cost","circuit theory","circuit-theory","landscape resistance*","landscape permeability"),
  outcomes=c("occup*","occurr*","abundan*", "species richness", "diversity", "speci* response*", "gene* diversity","gene flow", "speci* distribut*", "speci* densit*", "speci* composit*", "speci* assemblage*", "communit* composit*", "communit* assemblage*", "popul* persist*","popul* response*", "distribut* pattern*", "spatial* distribut*","movement*","dispersal"))

grouped_terms1

# grouped_terms2 <-list(
#   urban=c("urban","urban areas","urban landscapes","city","cities","towns"),
#   biodiversity=c("biodiversity","wildlife","vegetation","plants","trees","birds","avian","arthropods","insects","mammals","herptiles","reptiles", "amphibians"),
#   connectivity=c("connectivity", "landscape connectivity","functional connectivity","structural connectivity","habitat connectivity","ecological connectivity","habitat networks","ecological networks","corridors","habitat corridors","ecological corridors","least-cost","least cost","circuit theory","circuit-theory","landscape resistance","landscape permeability"),
#   outcomes=c("occupancy","occurrence","abundance", "species richness", "diversity", "species responses", "genetic diversity","gene flow", "species distributions", "species density", "species composition", "species assemblages", "community composition", "community assemblages", "population persistence","population responses", "distribution patterns", "spatial distributions","movements","dispersal"))
# 
# grouped_terms2

# 3. Writing boolean searches for databases ----------------------------------
# from the newly grouped terms
write_search(
  grouped_terms1,
  languages = "English",
  exactphrase = T,
  stemming = F,
  closure = "full",
  directory= "./output/final_searchstring_01", #saved to output folder
  writesearch = T
)
