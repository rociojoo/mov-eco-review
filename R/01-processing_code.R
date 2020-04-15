###############################################
# "Import queries using the refsplitr package"
###############################################
# The `resplitr` packages can handle bulk imports of WoS files if we give it the
# directory of the reference files. It then reads through each WoS
# file and pastes them into one data.frame.

### install from ropensci github page
# Note- We're importing two of our functions which are stored in the `R` folder.  
# Arguments are as follows:  
#   **path.fulltext** - Path to the folder with the full text downloads  
#   **path.pmc** - Path to the doi folder  
#   **path.save.stats** - place to store a summary stats text file  
#   **path.save.reviews** - place to store a file of papers to review  
#   **path.save.query** - a place to save the final output  
#   **name** <- the filename extension to add to our new outputs  

devtools::install_github('ropensci/refsplitr')
library(refsplitr)
path.fulltext <- "fulltext/"
path.pmc <- "Data/doi/"
path <- 'Data/ProcessedQueries/References/references_raw.csv' 
path.save.stats <- 'Data/ProcessedQueries/Stats/' 
path.save.reviews <- 'Data/ProcessedQueries/Reviews/' 
path.save.query <- 'Data/ProcessedQueries/References/'
name <- 'cleaned_papers_all_years'
source('R/CleaningQueries_meb.R')


# `Query-Results-WoK` is the folder we saved the bulk WoS files in. 
# `Dir = T` tells it to import all files in the directory.  
# `include_all = T` retains all columns from the original WoS reference.  

references <- references_read('Data/Query-Results-WoK',dir=T, include_all=T)
write.csv(references, 'Data/ProcessedQueries/References/references_raw.csv',row.names=F)

### Re-clean our querries by running them through the same word filter as in WoS. 
# This is because WoS is an imperfect word match and gives us more returns than we want.
cleaning.query(path,name,path.save.reviews,path.save.query,path.save.stats, path.pmc, path.fulltext)

###############################
# Creating dois and falsedois 
# to use as a uniquekey
###############################
# For this analysis we're going to us doi as a unique key, as later on the pdf/xml process
# will save papers using this doi. If we do not have a doi we will use the pmc to link to the doi. To get PMC conversion, go to https://www.ncbi.nlm.nih.gov/pmc/pmctopmid/. You'll want to create a table of pmcs and their dois.  

# Now read in the data created from cleaning.query and create the doi unique key column.

source('R/FalseDOIs.R')
data_all <- read.csv(paste0(path.save.query, name, "_full.csv"))
# Run the function which will add a new doi1 column we can merge on
out <- new_doi(data_all,path.pmc,path.fulltext)
# overwrite the file
write_csv(out,paste0(path.fulltext,"all_papers_full.csv"))
