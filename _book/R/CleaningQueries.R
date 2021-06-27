# This is the part that takes the outputs from WoK search and applies
# the same keyword filtering on those results, but this time in title,
# keywords and abstract

cleaning.query <- function(path, name, path.save.reviews, path.save.query, 
                           path.save.stats, path.pmc, path.fulltext) {
  require(dplyr)
  # path: where the WoK results are name: name of the file to be created
  # path.save.reviews: where review paper list will be saved
  # path.save.query: where file with all good papers will be saved
  # path.save.stats: where statistics of this filtering will be saved
  refs1 <- read.csv(path,stringsAsFactors = FALSE)
  refs1 <- subset(refs1, !DT %in% c('Editorial Material', 'Letter', 'Note', 'Correction', 'Meeting Abstract', 'News Item', 'Article; Data Paper', 'Reprint'))
  ind <- which(duplicated(as.character(refs1$TI)) == TRUE)  # weird, but it can happen in WoS to get duplicated papers in the list
  if (!dir.exists(path.save.stats)) {
    dir.create(path.save.stats, recursive = TRUE)
  }
  sink(paste0(path.save.stats, "stats-cleaning-", name, ".txt"), append = FALSE)
  cat(paste0("first amount of queries: ", nrow(refs1)))
  cat("\n")
  cat(paste0("duplicates: ", length(ind)))
  cat("\n")
  sink()
  if (length(ind) > 0) {
    refs1 <- refs1[-ind, ]
  }
  #refs1$AB <- as.character(refs1$abstract)
  
  #match_text <- paste0(moveco$title, moveco$abstract, moveco$keywords, moveco$extra.keywords,sep=', ')
  match_text <- paste0(refs1$TI, refs1$AB, refs1$DE, refs1$ID,sep=', ')
  ## check each match combination (explained in README file in Repository)
  words.maybe.not <- c("\\bcell", "\\bDNA", "enzyme", "\\bstrain", "neuro", "atom", 
                       "molecule", "lymph", "cortex", "cortic", "receptor", "patient", 
                       "prosthese", "\\beye", "particle", "tectonic", "\\bcounsel", "\\bcognit", 
                       "market", "\\bspine", "questionnaire", "sedentary", "insulin", "peristal", 
                       "muscle", "amput", "nervous", "retinal", "psychiatric", "disease", 
                       "virus", "remotely-guided", "tissue","polymer",'\\bmri\\b')
  match.maybe.not <- grep(paste(words.maybe.not, collapse = "|"), match_text, 
                          ignore.case = TRUE, value = FALSE)
  words.3a <- c("telemetry", "geolocat", "biologg", "accelerom", "geo-locat", 
                "bio-logg", "bio-logg", "reorient", "vhf", "argos", "radar", "sonar", 
                "\\bgls\\b", "vms", "animal-borne", 'tortuosity')
  match.maybe.yes <- grep(paste(words.3a, collapse = "|"), match_text[match.maybe.not], 
                          ignore.case = TRUE, value = FALSE)
  maybe.yes <- match.maybe.not[match.maybe.yes]  # the lines that should be kept 
  discard <- which(!match.maybe.not %in% maybe.yes)  # the other lines
  
  test.3 <- refs1[-match.maybe.not[discard], ]
  if(length(discard)==0) test.3 <- refs1[, ]
  
  words.1 <- c("behavio")
  words.2 <- c("moveme", "\\bmoving", "motion", "spatiotemporal", "kinematics", 
    "spatio-temporal")
  words.3 <- c("telemetry", "geolocat", "biologg", "accelerom", "gps", 
    "geo-locat", "bio-logg", "reorient", "vhf", "argos", "radar", "sonar", 
    "\\bgls\\b", "vms", "animal-borne")  # AIS and satellite
  words.4 <- c("animal", "individual", "human", "person", "people", "player", 
    "wildlife", "fishermen")
  match_text <- paste0(test.3$TI, test.3$AB, test.3$DE, test.3$ID,sep=', ')
  #any 3
  results <- data.frame(cat1=grepl(words.1, match_text,ignore.case=T),
    cat2=apply(sapply(words.2, function(x) grepl(x, match_text,ignore.case=T)),1,any),
    cat3=apply(sapply(words.3, function(x) grepl(x, match_text,ignore.case=T)),1,any),
    cat4=apply(sapply(words.4, function(x) grepl(x, match_text,ignore.case=T)),1,any)
  )
  
  results$sum <- apply(results, 1,sum)  
  results$refID <- test.3$refID
  results$row_number <- seq_along(test.3$refID)
  
  sink(paste0(path.save.stats, "stats-cleaning-", name, ".txt"), append = TRUE)
  cat(paste0("pass R filter: ", sum(results$sum>2)))
  cat("\n")
  sink()
  refs <- test.3[results$sum>2,]

  # there were some weird journals that appeared in our list (some of
  # them weren't even real journals)
  weird.journals <- c("DRIVER AND VEHICLE MODELING", "FREIGHT TRANSPORTATION RESEARCH: FREIGHT TRANSPORTATION (MULTIMODAL)", 
                      "TRANSPORTATION DATA, STATISTICS, AND INFORMATION TECHNOLOGY: PLANNING AND ADMINISTRATION", 
                      "TRAVEL DEMAND AND LAND USE 2003: PLANNING AND ADMINISTRATION")
  ind.weird <- which(as.character(refs$SO) %in% weird.journals == 
                       TRUE)
  sink(paste0(path.save.stats, "stats-cleaning-", name, ".txt"), append = TRUE)
  cat(paste0("papers from weird journals: ", length(ind.weird)))
  cat("\n")
  sink()
  if (length(ind.weird) > 0) {
    refs <- refs[-ind.weird, ]
  }
  ################
  # Now find dois
  # the data can be anything, as long as its eventually stored as a character vector, and it has abstract, title, keywords and doi columns
  #data_all <- read_csv(paste0(path.metadata,paper.title))
  # data_all <- read_csv(paste0(path.metadata,'all_papers_Journals-2000-2008.csv'))
  # Table with DOI conversion based on PMC (source https://www.ncbi.nlm.nih.gov/pmc/pmctopmid/)
  pm.doi.1 <- read.csv(paste0(path.pmc,"pmcidTOdoi.csv"), stringsAsFactors = FALSE)
  pm.doi.2 <- read.csv(paste0(path.pmc,"ids.csv"), stringsAsFactors = FALSE)
  pm.doi <- full_join(pm.doi.1,pm.doi.2)
  rm(pm.doi.1,pm.doi.2)
  refs <- new_doi(data_all =refs ,pm.doi, path.fulltext)
  ## first, identifying reviews and saving list in a file
  words.avoid <- c("this review", "we review", "introductory article", 
    "by reviewing", "our overview", "we summarize a review", "briefly reviewed")
  test.avoid <- unique(unlist(lapply(1:length(words.avoid), function(i) {
    match.avoid <- grep(words.avoid[i], refs1$AB, ignore.case = TRUE)
    return(match.avoid)
  })))
  sink(paste0(path.save.stats, "stats-cleaning-", name, ".txt"), append = TRUE)
  cat(paste0("potential reviews: ", length(test.avoid)))
  cat("\n")
  sink()
  if (!dir.exists(path.save.reviews)) {
    dir.create(path.save.reviews, recursive = FALSE)
  }
  reviews_simple <- refs[test.avoid, c('AF','TI','J9','ID','DE','AB','PU','PD','PY','doi2','C1','RP','doi_methods')]
  colnames(reviews_simple) <- c('author', 'title', 'journal', 'keywords', 'extra_keywords',
    'abstract','publisher','pubdate', 'pubyear', 'doi', 'affil', 'affil2','doi_methods')
  write.csv(reviews_simple, file = paste0(path.save.reviews, 
    "Reviews-", name, ".csv"))
  #refs$abstract <- refs$AB
  # str(refs)
  #refs3 <- refs[, -which(colnames(refs) == "AB")]
  if (!dir.exists(path.save.query)) {
    dir.create(path.save.query, recursive = FALSE)
  }
  write.csv(refs, paste0(path.save.query, name, "_full.csv"))
  
  refs_simple <- refs[,c('AF','TI','J9','ID','DE','AB','PU','PD','PY','doi2','C1','RP','doi_methods')]
  colnames(refs_simple) <- c('author', 'title', 'journal', 'keywords', 'extra_keywords',
    'abstract','publisher','pubdate', 'pubyear', 'doi', 'affil', 'affil2','doi_methods')
  write.csv(refs_simple, paste0(path.save.query, name, "_simple.csv"))
  
  print(paste0("Query:", dim(refs1)[1]))
  print(paste0("Final:", dim(refs)[1]))
  sink(paste0(path.save.stats, "stats-cleaning-", name, ".txt"), append = TRUE)
  cat(paste0("papers kept: ", nrow(refs)))
  cat("\n")
  sink()
}

#cleaning.query(path,name,path.save.reviews,path.save.query,path.save.stats)
