# Literature search preparation -------------------------------------------
library(remotes) #install.packages('remotes') #lets you download R packages outside of CRAN. Use as needed
remotes::install_github("rmetaverse/metaverse") #this installs a whole set of meta-analysis related R packages, https://rmetaverse.github.io/
#Relevant packages; litsearchr, revtools, synthesisr, metaDigitise, robvis. Load as needed.
library(tidyverse)
library(litsearchr)
library(wordcloud)

# Importing results from initial search string ----------------------------
naive_results<-import_results(directory="raw_data/naive_search") #Downloads all files in folder if no specific file is referenced
#check the data structure/format
str(naive_results)
colnames(naive_results)

# Generating potential search terms
## from keywords
naive_results$keywords %>% is.na() %>% sum() #351 results missing keywords
extract_terms(keywords = naive_results$keywords, 
              method="tagged", #keywords used by authors in the article
              min_freq = 5, #Only keywords used >5 times
              min_n = 1, 
              max_n = 4) #1-4 word long keywords
### visualize it in a wordcloud?

## from Titles and Abstracts
extract_terms(text=naive_results[, "title"], 
              method="fakerake", #using a modified RAPID AUTOMATIC KEYWORD EXTRACTION
              min_freq=3, #
              min_n=2)
