##
# calling packages that we will need
library(tidyverse)
library(dplyr)

# Indicating paths of dictionary, fulltext data, papers metadata, doi-pmc information, and where we will save the
# dictionary data.frame results and the statistics for each category in each dictionary

path.dict <- "./Data/Dictionary/csv-updated-versions/" # dictionaries need to be csv files
path.fulltext <- "./Data/fulltext/"
path_papers_summary <- "./Data/ProcessedQueries/References/"
path.save.stats <- "./Data/Dictionary/Stats/"
path.save.dict.results <- "./Data/Dictionary/Papers-Term/"
path.R <- "./R/"
path_taxo_data <- "./Data/ProcessedQueries/Taxonomy/"
path_synonyms <- "./Data/Dictionary/"

source(paste0(path.R,"Dictionary_Functions.R"))

# extracting the names of the files with methods sections:
files_methods <- dir(path.fulltext,pattern="methods.RData")
files_methods_name <- substr(files_methods, 1, nchar(files_methods) - nchar("_methods.RData"))

# data.frame with info on whether a dictionary is for the methods section (1) or only metadata (0):
dict.rules <- data.frame(dictionary = c("Data","Methods","Topics","Software","Framework"),
                         methods = c(1,1,0,1,0))

papers <- read.csv(file = paste0(path_papers_summary,"cleaned_papers_all_years_simple.csv"))

data_all <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

data_all$doi_methods <- as.character(data_all$doi_methods)

files_methods_use <- files_methods[files_methods_name %in% data_all$doi_methods]

############### RUNNING ON A DICTIONARY

# Dictionaries should be named as: "Dictionaries-(type-of-dictionary).csv"
# (type-of-dictionary): Data, Methods, Topics, Software or Framework

dictionary <- "Framework"   # "Data" # "Methods" # "Topics" # "Software" # "Framework"

count_terms_dictionary(data_all, files_methods=files_methods_use, path.dict, dictionary, dict.rules,
path.save.dict.results, path.save.stats,path.fulltext)

load(paste0("./Data/Dictionary/Papers-Term/paper-term-dictionary-",dictionary,".RData"))

file.show(paste0(path.save.stats,"dictionary-",dictionary,".txt"))
## All files with several keywords duplicated in the counts

## For taxa:

taxo_dictionary_format(path_taxo_data, path_synonyms, data_decade=data_all, path.save.dict.results, 
                                   path.save.stats)
file.show(paste0(path.save.stats,"dictionary-Taxonomy.txt"))

file.show(paste0(path.save.stats,"dictionary-Humans.txt"))
