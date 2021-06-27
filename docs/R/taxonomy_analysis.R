#####################################
# Extracting species names from text
# Using ITIS
######################################

## This code connects to an already downloaded ITIS database and uses it to search
# our papers for taxonomic names and matches those names to their taxa.
# Load packages
library(dplyr)
library(dbplyr)
library(RSQLite)
library(cowplot)

#############################
# Variables
#############################
# Load taxonomy functions
source("R/taxonomy_fun.R")

# Load papers
papers <- read.csv('Data/ProcessedQueries/References/cleaned_papers_all_years_simple.csv',stringsAsFactors = FALSE)

# Connect to ITIS database
path <- "/home/matt/Downloads/itisSqlite022618/"
taxonomy <- DBI::dbConnect(RSQLite::SQLite(), paste0(path,"ITIS.sqlite"))

# search variables 
# Our human search words (in rexpr)
# Some words were not included because they were problematic
# like human (ex human dominated landscape)
# Tourist (ex tourist areas)
# driver (obvious)
humans <- c("players?", "patients?", "\\bchild\\b", "children", "teenagers?", 
  "\\bpeople\\b", "students?", "fishermen", "\\bperson\\b", "tourists?",  
  "visitors?", "\\bhunters?\\b", "customers?", "\\brunners?",
  "participants?", "\\bcyclists?", "employees?", "hikers?", "athletes?", 
  "\\bboys?\\b", "girls?", "woman", "women", "\\bman\\b", "\\bmen\\b", "adolescents?")
# Our problematic common names, exact match not reg ex. These match the database common vernacular.
# And was built as we found more problematic names and words
bad_names <- c('scales','whites','animals','scats','blues','coppers','Blues','fliers','grunts','scavengers', 'soles','spot','spots','sole',
  'permit','permits','rainbow','rainbows','Whites','whites','white','ray','lion','Lion','Ray','Here','here','Mexico','mexico','delta','Delta',
  'prominents','infant','Infant','purple','purples','emperors','emperor', 'underwings','underwing','Alabama','Alabama','Car', 'Goes','Corolla',
  'corolla','Helix','helix','Planes','plane','Fota','brother','scat', 'Gyros','gyros','gyro','gyros','runners','cuckoos','Ego','Gila')

####################################
# Start
####################################
# Get table of taxonomic units
tax_units <- tbl(taxonomy, "taxonomic_units") %>% collect()

# Seperate titles, keywords, and abstracts
papers$keywords[is.na(papers$keywords)]<-''
papers$extra_keywords[is.na(papers$extra_keywords)]<-''

titles <- papers$title
keywords <- paste0(papers$keywords,'; ',papers$extra_keywords)
abstracts <- papers$abstract

# Only keep species level names in Animalia

animalia <- tax_units %>%
  filter(kingdom_id==5) %>%
  filter(rank_id==220) %>%
  filter(name_usage=="valid") %>%
  collect()

# Exclude humans

animalia <- animalia[!animalia$complete_name == "Homo sapiens",]

# Get vernaculars table
vernaculars <- tbl(taxonomy, "vernaculars")

# Select common names in English and tsn
common_names <- vernaculars %>%
  filter(language=="English") %>%
  select(tsn, vernacular_name) %>%
  collect()

# Join to table of scientific names by tsn
animals <- left_join(animalia, common_names, by="tsn") %>%
  select(tsn, complete_name, vernacular_name, parent_tsn, rank_id)

# Create the search vector to loop through. We're going to combine
# gensu_species names as well as their common vernaculars.
# Take out commonly bad common names.

animals <- animals %>% filter(!vernacular_name %in% bad_names)
complete <- animals$complete_name
vernacular_name <- animals$vernacular_name

# We're using regexpr here to make everything much easier

search <- paste0('\\b',complete,'s?\\b|\\b',vernacular_name,'s?\\b')
search[is.na(vernacular_name)] <- paste0('\\b',complete[is.na(vernacular_name)],'s?\\b') 
search <- gsub('ss\\?','s?', search)
ids <- animals$tsn

# Run the function!
#list2 <- lapply(ids, search_function, data = ids)
all_sn <- search_function(data = search, ids = ids)
colnames(all_sn)[colnames(all_sn)=='id'] <- 'tsn'

##############################
# None genus_species level names
# These codes can be combined for simplicity
# if necessary. 

# Select common names in English and tsn
common_names <- vernaculars %>%
  filter(language=="English") %>%
  select(tsn, vernacular_name) %>%
  collect()

high_taxa <- tax_units %>%
  filter(kingdom_id==5) %>%
  filter(rank_id!=220) %>%
  filter(name_usage=="valid") %>%
  collect()

htaxa <- pull(high_taxa, complete_name)

# Join to table of scientific names by tsn
high <- left_join(high_taxa, common_names, by="tsn") %>%
  select(tsn, complete_name, vernacular_name, parent_tsn, rank_id)
# Exclude some common names that are problematic

high <- high %>% filter(!vernacular_name %in% bad_names)
ids <- high$tsn
complete <- high$complete_name
vernacular_name <- high$vernacular_name
search <- paste0('\\b',complete,'s?\\b|\\b',vernacular_name,'s?\\b')
search[is.na(vernacular_name)] <- paste0('\\b',complete[is.na(vernacular_name)],'s?\\b') 
search <- gsub('ss\\?','s?', search)
#list3 <- lapply(ids, search_function, data = ids)
all_ht <- search_function(data = search[2300:3000], ids = ids)
colnames(all_ht)[colnames(all_ht)=='id'] <- 'tsn'
################################
# Putting it all together
# Species level

all_sn$level <- 'genus_sp'
all_ht$level <- 'higher'

all_all <- bind_rows(all_sn, all_ht) %>% 
  filter(!is.na(paper_id)) %>% 
  distinct()

taxa_tsn <- all_all %>% 
  distinct(tsn)
# We now need to derive the class and order of each name
ll <- lapply(taxa_tsn$tsn[],getClassOrder)
taxa_class_order <- as.data.frame(do.call(rbind,ll))

all_all <- merge(all_all,taxa_class_order, by='tsn',all.x=T)
all_all <- merge(all_all,common_names, by='tsn',all.x=T)

# Filter out duplicate rows
all_all <- all_all %>% distinct(tsn, paper_id,section,level,.keep_all=T)
all_all$doi2 <- papers$doi2[all_all$paper_id]

# Write it out
write.csv(all_all, 'animals_in_papers.csv',row.names = FALSE )

# human search
# This is exponentially faster
human_df <- search_function(data=humans, terms=T)
human_df <- human_df[!is.na(human_df$paper_id), ]
human_df$doi2 <- papers$doi2[human_df$paper_id]
write.csv(human_df, 'humans_in_papers.csv',row.names = FALSE )
##########
