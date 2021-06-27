
library(magrittr)
library(dplyr)
library(tidytext) # to use unnest_tokens
library(tidyr) # to use separate

###  Read in arguments
# Arguments are as follows : 
#   **path.fulltext** : path for material and methods sections
#   **path_papers_summary** : path for cleaned WoS reference csv   

path.fulltext <- "./Data/fulltext/"
path_papers_summary <- "./Data/ProcessedQueries/References/"

# extracting the names of the files with methods sections:
files_methods <- dir(path.fulltext,pattern="methods.RData")
files_methods_name <- substr(files_methods, 1, nchar(files_methods) - nchar("_methods.RData"))
papers <- read.csv(file = paste0(path_papers_summary,"cleaned_papers_all_years_simple.csv"))
data_all <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)
data_all$doi_methods <- as.character(data_all$doi_methods)
files_methods_use <- files_methods[files_methods_name %in% data_all$doi_methods]

scrunched.methods <- lapply(files_methods_use, function(x) {
  load(paste0(path.fulltext, x))
  data <- methods_vector
  data <- as.character(paste0(data, collapse = " "))
  # a dataframe of troublesome symbols and their replacements
  help.frame <- data.frame(
    problem = c("’", "–"),
    solution = c("'", "-")
  )
  
  for (p in seq_len(nrow(help.frame))) {
    data <- gsub(help.frame$problem[p], help.frame$solution[p], data, perl = T)
  }
  return(data)
})
data <- unlist(scrunched.methods) # now these are the methods

test <- data.frame(abstract = data, doi = files_methods_use)
test$abstract <- as.character(test$abstract)

## trigrams
## 
austen_trigrams <- test %>%
  unnest_tokens(bigram, abstract, token = "ngrams", n = 3)
# austen_trigrams
# austen_trigrams %>%
#   count(bigram, sort = TRUE)

trigrams_separated <- austen_trigrams %>%
  separate(bigram, c("word1", "word2","word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

# new bigram counts:
trigram_counts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE) %>% 
  filter(n > 1)

trigram_counts

# saveRDS(trigram_counts,file="trigrams.rds")

# for a cleaner list, filter out numbers, symbols, plural, etc. I just cleaned them by hand separately.
