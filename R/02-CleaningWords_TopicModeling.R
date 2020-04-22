# PATHS
path_data <- "./Data/ProcessedQueries/References/" #path to the file with the summary information of the papers, from the queries of WoK and processed
# that file is called "cleaned_papers_all_years_simple.csv"
path_dict_tools <- "./Data/Dictionary/AuxiliaryTextMining/" # path to some auxiliary files for text mining

###########

# LIBRARIES
library(tidyverse)
library(tidytext)
library(stringr)
library(tm) # removing words in Spanish
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("trinker/textstem")
library(textstem) # lemmatizing # to install textstem, error when installing statnet.common because my R version is 3.4, so had to work with 4.1.4 statnet.common

# calling auxiliary functions
source("./R/Americanizing.R")
source("./R/cleaning_words_abstract.R")

############

papers <- read.csv(paste0(path_data,"cleaned_papers_all_years_simple.csv"), stringsAsFactors = FALSE)

datosOriginales <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

# DATA OF INTEREST --------------------------------------------------------

# 1.Filter columns of interest
test <- datosOriginales %>% 
  mutate(paper = doi) %>% 
  select(paper, abstract) 

# now using functions to clean abstracts:
df_words <- cleaning_words_abstract(test, path_dict_tools)

# let's how many of each there are (so maybe filter the very infrequent ones out - the ones that appear once)
word_freq <- df_words %>% count(word_am_lem)
word_freq <- word_freq %>% mutate(prop = n/sum(n))

terms_extract <- data.frame(word_am_lem = 
                              word_freq$word_am_lem[which(word_freq$prop < round(min(word_freq$prop),10) + 10^{-10})],
                            stringsAsFactors = FALSE)
df_words <- df_words %>% 
  anti_join(terms_extract)


# FINALLY
# TIDY DATA
datosFinales <- df_words %>% 
  select(paper, word_am_lem)
names(datosFinales) <- c("paper","word")


# datosFinales <- datos3 %>% 
#   select(paper, word)

# Save data in the project
write.csv(x = datosFinales, file = paste0(path_data,"cleaned_filtered_TidyData_TopicModeling.csv"), row.names = FALSE)


