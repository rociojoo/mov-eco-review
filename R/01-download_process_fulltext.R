###################################################
# "Downloading and processing full text pdfs/xmls"
##################################################
#  We're going to download and process texts in two stages.  
# 1. Download pdf/xmls  
# 2. Pull out the method sections and create Rdata files from methods and full texts.  


## Download fulltext files
# We're going to use the "fulltext" package to download pdf and xml versions of our papers
# You need an Elsiever, Springer, Wiley, and PLOS key in order to access most of these papers. 
# Which we acquired from the UF library.  
# Our arguments are:  
#   path_fulltext - location the Rdata and raw pdfs will be saved. (raw files are in the /raw section)  
#   path.pmc - A csv created of missing papers with pmcs that do not have dois.  

library(fulltext)
library(stringr)
library(tidyverse)
## args
path_fulltext <- "/home/matt/r_programs/MovEcoReview-Repository/Data/fulltext"
path.pmc <- "/home/matt/r_programs/MovEcoReview-Repository/Data/doi/"

#########################################
## First read in the cleaned papers files
papers <- read.csv('Data/ProcessedQueries/References/cleaned_papers_all_years_full.csv')
# subset papers to our specific timeline as we did not do this in WoS originally
papers <- subset(papers, PY>=2008)
doi <- papers$DI
doi_character <- as.character(doi)
ind_doi <- which(doi_character != "") # some don't come with doi info


# We'll need to know where the package is going to be downloading papers.
# Its normally in a tmp folder on your computer. We later manually copy these files 
# over to the 'path_fulltext' location.
# 
# Check the `cache_path` object to tell where the temp folder is that the files 
# are being downloaded too and keep it for later.

x <- 1
res <- ft_get(doi_character[x])
publish <- names(res)
cache_path <- res[[1]]$data$cache_path


# We're assuming we've already downloaded some papers, and only want to download new papers. 
# This code will check if any papers already exist in our path_fulltext.


list_downloaded <- list.files(path = path_fulltext, pattern = "_fulltext.RData")
dois_downloaded <- unlist(lapply(strsplit(list_downloaded, "_fulltext"), `[[`, 1))
dois_list <- str_replace_all(doi_character[ind_doi],"[[:punct:]]", '_')
dois_list_missing <- which(!dois_list %in% dois_downloaded == TRUE)

# maybe some of them are not their because the file name is in PMC and not DOI

pm.doi.1 <- read.csv(paste0(path.pmc,"pmcidTOdoi.csv"), stringsAsFactors = FALSE)
pm.doi.2 <- read.csv(paste0(path.pmc,"ids.csv"), stringsAsFactors = FALSE)
pm.doi <- full_join(pm.doi.1,pm.doi.2)
rm(pm.doi.1,pm.doi.2)

dois_missing <- doi_character[ind_doi[dois_list_missing[which(!doi_character[ind_doi[dois_list_missing]] %in% pm.doi$DOI == TRUE)]]]

# if I haven't downloaded papers before, just do dois_missing <- dois_character[ind_doi]

# Now to start download. Again you'll have to get the keys from your library or equivalent

ELSEVIER_SCOPUS_KEY <- "my_elsevier_key"
SPRINGER_KEY <- "my_springer_key"
WILEY_KEY <- 'my_wiley_key'
PLOS_KEY <- 'my_plos_key'
fail <- list()
for (i in 1:length(dois_missing)) {

res <-tryCatch(ft_get(dois_missing[i],
scopusopts=list(key=ELSEVIER_SCOPUS_KEY),
elsevieropts=list(key=ELSEVIER_SCOPUS_KEY),
wileyopts=list(key=WILEY_KEY), 
bmcopts=list(key=SPRINGER_KEY),
plosopts=list(key=PLOS_KEY)), error=function(w)NA)
if (is.na(res)) fail[[paste0(i)]] <- NA
}


###########################################
# Converting pdf and xml files to Rdata files, 
# while seperating out Methods sections
###########################################
# We'll start with pdfs.  
# Arguments for the following anaylysis are:  
#   **path.fulltext.raw** is where you placed the pdf xmls after you moved them from the cache.  
#   **path.fulltext** are where you want the RData files to be outputted. After this is all done and youre sure youve finished with the pdf/xmls youll want to delete the raw folder if its to be uploaded to github.  

### PDF

path.fulltext.raw <- 'fulltext/raw/'
path.fulltext<- 'fulltext/'
source('R/pdf-analysis.R')
pdf_analysis(path.fulltext.raw, path.fulltext)

#### Now XML

path.fulltext.raw <- 'fulltext/raw/'
path.fulltext<- 'fulltext/'
source('R/xml-analysis.R')
xml_analysis(path.fulltext.raw, path.fulltext)

