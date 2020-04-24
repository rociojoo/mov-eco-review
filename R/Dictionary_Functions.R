######################################################################################
# Functions
######################################################################################
# This function translates our dictionary terms into perl expression and then searches
# our character vector for matches.
term.searching<-function(data, dict){
  #we're wrapping this in an lapply for speed
  lapply(dict, function(terms){
    
    terms <- as.character(terms)[terms!=''] # takes out empty space
    terms <- terms[!duplicated(terms)] # takes out duplicates
    terms <- gsub('\\(',"\\\\(",terms)
    terms <- gsub('\\)',"\\\\)",terms)
    #compiling code into perl for regex matching. We run off of three pieces of logic
    # the or statements( `|`) SHOULD work without needing to compile as they are regexpr
    # The ('&') gets converted to the pearl equivalent aside by side not, it contains no look around I believe.
    
    terms<-sapply(terms,function(x){
      # x<-terms[1]
      x<-ifelse(grepl('&',x),paste0("(?=.*",unlist(strsplit(x,'&')),')',collapse=''),x)
      #x<- gsub('\\^|\\$','\\\\b',x)
      # WE're going to add in a line where if its an exact match phrase it partial matches as well. This
      # Means its important to use ^ and $ to stop this functioning. For it we add '+' on the end of every
      # word that is not being broken by ^ or $
      if(any(grepl('%',x))){
        x<- paste0(sapply(unlist(strsplit(x,' ')), function(x) ifelse(grepl("\\$",x), gsub("\\$",'\\\\b',x), paste0(x,'+',collapse='') )), collapse=' ')
      }else{
        x<-gsub("\\$",'\\\\b',x)
      }
      x<-gsub("\\^",'\\\\b',x)
      # The pattern we're trying to create is, where we want to match 'snow' but not 'snow goose' 
      # '^(?!.*snow goose).*snow.*$'
      if(grepl('!',x)){
        x<-sapply(unlist(strsplit(x,'\\|')), function(y) 
          ifelse(grepl('!',y),
                 paste0("^(?!.*(",
                        paste(unlist(strsplit(y,'!'))[-1],collapse='|'),')).*',
                        unlist(strsplit(y,'!'))[1],
                        ".*$",
                        collapse=''),
                 y))
        x<-paste0(x,collapse='|')}
      return(x)
    })
    # need to split page breaks up
    data<-gsub('\n',' ',data)
    terms<-gsub('\n',' ',terms)
    #
    result.list<-list() # I suppose this should be put into a lapply as well
    for(i in seq_along(data)){
      #i<-4
      sub.data<-data[i]
      text<-as.character(sub.data)
      # We have decided to use the % symbol to mean be case sensitive, we've turned it off otherwise
      # Theres two ways to do this, one the (?i) makes things case insensitive in perl. If you 
      # put it infront of a phrase it will match that phrase case insensitive
      # Or we can just run the grepl function as case sensitive or not depending on the word
      # Ive chosen the latter, but it should be noted we can do the other
      result.list[[paste0(i)]]<-sapply(terms, function(x) ifelse(any(grepl('%',x)),grepl(gsub('%','',x), text,perl=TRUE),grepl(x, text,ignore.case=TRUE,perl =TRUE)))}
    # The output is a list of true and falses. 
    results<-as.data.frame(do.call(rbind, result.list))
    return(results)
  })
}
# Matt's comments on last version:
# So issues I fixed:
#  -line breaks = '\n' gets translated to spaces.
#-^word$ is now being translated correctly into the appropriate reg ex. I used \b as its basically the same.
# ^ is actually reserve for the beginning of the character string, which is literally just the first word of the text.
# Same with $, so its being translated into word blocks which is \bword\b which basically does what youre asking
# -'!' was not correctly working as I thought. It's being translated to a little more complex regex.
# Largely because I had not tested for it to work when youre searching for the phrase 'snow' but not 'snow goose'.
# The way it was written would work IF the words were not the same, in this case 'snow'.
# So it only worked if you wrote snow ! goose.
# So i translated it correctly now, and it seems to work.
# The one caveat is if you use the '!' you cant also use '|'.
# So in tommys example he needs to type 'snow ! goose', 'snow!geese', 'snow!hare' in new cells.
# Its simply too hard to translate to regex otherwise
# About &: If you guys decide you want it to search WITHIN a sentence,
# well I would first beg y'all to think really hard if its worth the time to write that,
# and if theres an easier way to create words and phrases to search what you want.
# That would mean we'd have to split everything into sentences, and then search for terms within sentences.
# Which obviously can be done. But you're increasing complexity.

# # function to recover the doi information in the name of the method files (pseudo dois because we will not have the
# # '.', '/', "_" symbols) # I DON'T NEED THIS ANYMORE!
# pseudo_doi_downloaded_methods <- function(files_methods, pm.doi) {
#   res_name <- lapply(1:length(files_methods), function(x) {
#     load(paste0(path.fulltext, files_methods[x])) # it's called methods_vector
#     file_name <- substr(files_methods[x], 1, nchar(files_methods[x]) - nchar("_methods.RData"))
#     return(file_name)
#   })
#   # taking file names
#   files <- unlist(lapply(res_name, "[[", 1))
#   ## MB Weird approach to load and substring in the same loop, and then
#   ## MB extract the first element. What about (replaces previous 6 lines):
#   ## MB (note the use of 'sub' (not 'gsub') instead of substr)
#   ## MB   lapply(files_methods, function(fmi) load(paste0(path.fulltext, fmi)))
#   ## MB   files <- sub("_methods.RData", "", files_methods)
#   # Take all the names that are not DOIs
#   ind_doi <- grep("10_", files)
#   doi <- files[ind_doi]
#   ind_not_doi <- which((files %in% doi) == FALSE)
#   not.doi <- files[ind_not_doi]
#   ## MB Little bit of code rationalization to replace previous 3 lines:
#   ## MB   ind_doi <- grepl("^10_", files)
#   ## MB   doi <- files[ind_doi]
#   ## MB   not.doi <- files[!ind_doi]
#   # New order for data.frame corresponding to the dois:
#   new_order <- c(ind_doi, ind_not_doi)
# 
#   # Get a list of PMC identifiers (some names come with PMC instead of DOIs so we need to get the corresponding DOIs)
#   pm.id <- substr(not.doi, 1, 7) ## MB Are there PMCs with more or less than 7 characters?
#   pm.doi$pmcid_transf <- substr(pm.doi$PMCID, 4, nchar(pm.doi$PMCID))
#   # crossing info
#   pm.id <- data.frame(PMID = pm.id)
#   pm.doi <- left_join(pm.id, pm.doi, by = c("PMID" = "pmcid_transf"))
#   ## MB Not sure I understand the left join here… Aren't you simply
#   ## MB subsetting the DOIs from pm.doi that are present in pm.id? You
#   ## MB could replace the previous two lines (and the $DOI part in the
#   ## MB following) with:
#   ## MB   pm.doi <- pm.doi[pm.doi$pmcid_transf %in% pm.id, "DOI"]
#   doi <- gsub("_", "", doi) # get rid of _s
#   # Get rid of separators in the pm.doi$DOI column
#   pm.doi$DOI <- str_replace_all(pm.doi$DOI, "[[:punct:]]", "")
#   # Put together all DOIs for all papers that we were able to download
#   doi.downloaded <- c(doi, pm.doi$DOI)
# 
#   return(list(pseudo_doi = doi.downloaded, new_order = new_order))
# }

# This function calls the term.searching function and applies it to titles, keywords and abstract, and also methods
# when required.
# Running this function for each dictionary gives as a result a .RData file with a dataframe where the each row is
# a paper, and each column is a keyword + a column with a pseudo DOI.
# Pseudo DOI because it is either a compact version of the DOI (no underscore, slash or dot).
# The values in this data.frame (for the keyword columns) are 0 (the keyword is not present in the paper)
# or 1 (it is present).
# The code also generates a file with statistics of number of papers related to at least one keyword,
# at the number of papers related to each category (the headers in each dictionary).

count_terms_dictionary <- function(data_all, files_methods, path.dict, dictionary, dict.rules, path.save.dict.results, path.save.stats, 
                                   path.fulltext = NULL, suffix = NULL) {
  ## MB Would be really nice to make good use of spaces in the
  ## MB code. There are existing solutions to automate this, such as
  ## MB styler, use them!
  ## MB https://styler.r-lib.org/index.html

  if (any(dict.rules$dictionary %in% dictionary)) {
    ## MB Can 'dictionary' be multiple? If not, what about simply:
    ## MB if (dictionary %in% dict.rules$dictionary) {

    dict.frame <- read.csv(paste0(path.dict, "Dictionaries-", dictionary, ".csv"), stringsAsFactors = FALSE)
    ## MB Since you're in the tidyverse, why not using readr::read_csv?
    ## MB This said, if we are going to package this, probably better
    ## MB to not rely on the tidyverse, or as less as possible.

    dict_gather <- dict.frame %>%
      gather(key = category, value = term) %>%
      filter(term != "") %>%
      filter(!duplicated(term))

    frame.terms <- dict_gather$term ## MB Avoid creation of unnecessary objects (often better to use accessors [] or [[]])

    if (dict.rules$methods[dict.rules$dictionary %in% dictionary]) {
      ## MB Oddly, R does not need to test equality to 1. Could be tested as:
      ## MB   if (dict.rules$methods[dict.rules$dictionary %in% dictionary]){
      ## MB BTW, the condition above implies that 'dictionary' is of length 1…

      scrunched.methods <- lapply(files_methods, function(x) {
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

      ## MB Again, avoid unnecessary objects. Could be written as:
      ## MB   scrunched.methods <- lapply(files_methods, function(x)
      ## MB   {
      ## MB       load(paste0(path.fulltext, x))
      ## MB       data <- as.character(paste0(methods_vector, collapse = " "))
      ## MB       gsub("’", "'", data, perl = TRUE)
      ## MB   })

      data <- unlist(scrunched.methods) ## MB 'c(unlist(…))' seems redundant.

      methods_count <- term.searching(data, dict = dict.frame) ## MB You can do the 'do.call' below here in one pass…

      # # get dois from methods:
      # res_doi <- pseudo_doi_downloaded_methods(files_methods, pm.doi)
      # 
      # res_doi <- gsub("[^[:alnum:]///' ]", "", 
      #                 substr(files_methods, 1, nchar(files_methods) - nchar("_methods.RData")))
      res_doi <- substr(files_methods, 1, nchar(files_methods) - nchar("_methods.RData"))
      

      methods_count <- do.call("cbind", methods_count)

      if (ncol(methods_count) != length(frame.terms)) {
        message(paste0("Error - Check your dictionary: you might have duplicated keywords"))
        ## MB This error should be caught before in 'terms.searching'
      } else {
        # methods_count <- methods_count[res_doi$new_order, ]
        # row.names(methods_count) <- res_doi$pseudo_doi ## MB As a general rule, never ever rely on row.names. They are way too subject to error… If you need an ID, make it a column.
        # methods_count <- as.data.frame(methods_count) ## MB If really necessary, you can use the row.names argument of 'as.data.frame'.
        # methods_count$doi <- res_doi$pseudo_doi
        methods_count$doi <- res_doi

        ## MB Same for all three objects below, could be simplified
        ## MB with much fewer assignments:
        ## MB   abstract_count <- do.call("cbind", term.searching(data_all$abstract,
        ## MB       dict.frame))
        data <- data_all$abstract
        abstract_count <- term.searching(data, dict.frame)
        abstract_count <- do.call("cbind", abstract_count)

        data <- data_all$title
        title_count <- term.searching(data, dict.frame)
        title_count <- do.call("cbind", title_count)

        data <- data_all$keywords
        keywords_count <- term.searching(data, dict.frame)
        keywords_count <- do.call("cbind", keywords_count)

        metadata_count <- 1 * abstract_count + 1 * title_count + 1 * keywords_count ## MB I wonder why you multiply every element by 1…

        row.names(metadata_count) <- data_all$doi
        metadata_count$doi <- data_all$doi
        metadata_count$doi_methods <- data_all$doi_methods
        metadata_count <- metadata_count[order(metadata_count$doi_methods), ]

        # rm(abstract_count, title_count, keywords_count) ## MB Why?

        # diff_doi <- setdiff(metadata_count$doi_methods, methods_count$doi) ## MB I start to be a bit lost here, but you know it only takes what is in metadata_ but not methods_, right? (i.e. not the other way around)
        
        # data_all dois can be grouped in 3 categories: 
        # 1) matching with methods filenames, 
        # 2) NAs (no method section), 
        # 3) PMCs that Matt added later, but no need
        # So I actually only care about the first group for merging
        
        methods_count <- methods_count[order(methods_count$doi), ]
        
        ind_methods_1 <- which((metadata_count$doi_methods %in% methods_count$doi) == TRUE)
        
        ind_methods_2_3 <- which((metadata_count$doi_methods %in% methods_count$doi) == FALSE)
        # metadata_count_methods <- 
        # test <- setdiff(metadata_count$doi_methods, methods_count$doi)
        # methods_secondpart <- as.data.frame(matrix(0, ncol = ncol(methods_count) - 1, nrow = length(diff_doi)))
        # methods_secondpart$doi <- diff_doi
        # row.names(methods_secondpart) <- diff_doi
        # colnames(methods_secondpart) <- colnames(methods_count)
        # methods_count <- rbind.data.frame(methods_count, methods_secondpart)
        ## MB Simpler version using dplyr::bind_rows, without the
        ## MB need to create a second object:
        
        doi_column <- metadata_count$doi
        # doi_methods_column <- metadata_count$doi_methods
        
        matrix_count_full <- rbind.data.frame(metadata_count[ind_methods_2_3,1:(ncol(metadata_count)-2)],
                                              metadata_count[ind_methods_1,1:(ncol(metadata_count)-2)] + 
                                                methods_count[,1:(ncol(methods_count)-1)])
        
        matrix_count_full[matrix_count_full > 1] <- 1
        # matrix_count_full$doi <- doi_column 
        matrix_count_full$doi <- c(as.character(doi_column[ind_methods_2_3]), as.character(doi_column[ind_methods_1]))
        
        # rownames(matrix_count_full) <- doi_column # I think I already did this...
        matrix_count_full <- matrix_count_full[order(matrix_count_full$doi), ]
        
        
           # methods_count_full  <- dplyr::bind_rows(methods_count,
           #     data.frame(doi = metadata_count$doi_methods[ind_methods_2_3]))
           # methods_count[is.na(methods_count)] <- 0
        # methods_count <- methods_count[order(methods_count$doi), ]

        # matrix_count <- 1 * metadata_count[, 1:(ncol(metadata_count) - 1)] + 1 * methods_count[, 1:(ncol(methods_count) - 1)] ## MB Still don't get the *1
        # matrix_count <- as.data.frame(matrix_count)
        # matrix_count$doi <- metadata_count$doi
        ## MB In one shot:
          # matrix_count <- data.frame(metadata_count[, 1:(ncol(metadata_count) -
          #      1)] + methods_count[, 1:(ncol(methods_count) - 1)],
          #      doi = metadata_count$doi)
        # rm(metadata_count, methods_count) ## MB Again: why?

        # matrix_count <- reshape2::dcast( ## MB Mamma mia! One call mixing reshape2 and plyr on top of dplyr! You're looking for trouble here, really… Can't believe there's no better way. And by the way, both reshape2 and plyr are not called by library, which says a lot already…
        #   dplyr::mutate(
        #     reshape2::melt(matrix_count, id.var = "doi"),
        #     value = plyr::mapvalues(value, c(0, 1, 2), c(0, 1, 1))
        #   ),
        #   doi ~ variable
        # )

        colnames(matrix_count_full) <- c(frame.terms,"doi")

        # return(matrix_count)
      }
    } else {
      data <- data_all$abstract
      abstract_count <- term.searching(data, dict.frame)
      abstract_count <- do.call("cbind", abstract_count)

      data <- data_all$title
      title_count <- term.searching(data, dict.frame)
      title_count <- do.call("cbind", title_count)

      data <- data_all$keywords
      keywords_count <- term.searching(data, dict.frame)
      keywords_count <- do.call("cbind", keywords_count)

      metadata_count <- 1 * abstract_count + 1 * title_count + 1 * keywords_count

      if (ncol(abstract_count) != length(frame.terms)) {
        message(paste0("Error - Check your dictionary: you might have duplicated keywords"))
      } else {


        # get dois here
        row.names(metadata_count) <- data_all$doi
        metadata_count$doi <- data_all$doi

        # rm(abstract_count, title_count, keywords_count)

        # abstract_count <- count_terms_in_metadata(frame.terms,data$abstract)
        # get dois here
        # row.names(abstract_count) <- data$doi

        # abstract_count <- as.data.frame(abstract_count)
        # abstract_count$doi <- data$doi
        # order_alpha <- order(abstract_count$doi)
        # abstract_count <- abstract_count[order_alpha,]
        metadata_count <- metadata_count[order(metadata_count$doi), ]

        # colnames(metadata_count) <- c(frame.terms,"doi")

        matrix_count_full <- cbind.data.frame(metadata_count$doi, metadata_count[, 1:(ncol(metadata_count) - 1)]) ## MB 'data.frame' achieves the same.

        colnames(matrix_count_full) <- c("doi", frame.terms)

        rm(metadata_count)
        # return(abstract_count)
      }
    }

    ## do one matrix by category
    df_term_count_2 <- matrix_count_full %>%
      gather(key = term, value = count, -doi)
    row.names(df_term_count_2) <- 1:nrow(df_term_count_2)

    matrix_CatTerm <- left_join(df_term_count_2, dict_gather) %>%
      mutate(CategoryTerm = paste0(category, " : ", term)) %>%
      select(doi, count, CategoryTerm) %>%
      spread(key = CategoryTerm, value = count)
    if (is.null(suffix)){
    save(matrix_CatTerm, file = paste0(path.save.dict.results, "paper-term-dictionary-", dictionary, ".RData"))
    }else{
      save(matrix_CatTerm, file = paste0(path.save.dict.results, "paper-term-dictionary-", dictionary,'-',suffix,".RData"))
    }

    ## Counts for each term
    term_counts <- colSums(matrix_CatTerm[, -1])

    ## Categories of terms
    category <- unlist(lapply(strsplit(colnames(matrix_CatTerm), " : "), "[[", 1))[-1]

    ## Cumulative count per category (= stats above)
    by(term_counts, category, sum)

    ## Remove duplicates (i.e. 1 count per paper per category):
    df_CatTerm <- data.frame(t(matrix_CatTerm[, -1]))
    colnames(df_CatTerm) <- matrix_CatTerm$doi
    df_term_count_3 <- by(df_CatTerm, category, function(ca) sum(colSums(ca) > 0))

    # df_term_count_3 <- left_join(df_term_count_2, dict_gather) %>%
    #   select(doi,count,category) %>%
    #   group_by(category) %>%
    #   summarize(counts=sum(count))

    papers_with_info <- sum((matrix_count_full %>%
      select(-doi) %>%
      rowSums()) > 0)
    if (is.null(suffix)){
    sink(paste0(path.save.stats, "dictionary-", dictionary, ".txt"), append = FALSE)
    }else{
      sink(paste0(path.save.stats, "dictionary-", dictionary, "-", suffix, ".txt"), append = FALSE)
    }
  
    cat(paste0("Number of papers from which information was extracted: ", papers_with_info))
    cat("\n")
    cat("\n")
    cat(paste0("Number of papers per category: "))
    print(df_term_count_3)
    cat("\n")
    sink()
  } else {
    message(paste0("Error - File not found: ", path.dict, "Dictionaries-", dictionary, ".csv"))
  }
}

########## FUNCTION TO PUT THE DATA ON SPECIES GROUPS AND HUMAN CATEGORIES IN THE SAME FORMAT AS THE OTHER DICTIONARY OUTPUTS ##########
taxo_dictionary_format <- function(path_taxo_data, path_synonyms, data_decade, path.save.dict.results, 
                                   path.save.stats){
  animals_data <- read_csv(paste0(path_taxo_data,"animals_in_papers.csv"))
  humans_data <- read_csv(paste0(path_taxo_data,"humans_in_papers.csv"))
  ## Working on animals dataset first
  
  # First, filtering: there are some repeated information at the class level in the file
  df_dups <- animals_data[c("class", "doi")]
  animals_no_dup <- animals_data[!duplicated(df_dups),]
  
  # Simplifying classes
  synonyms_keywords <- read_csv(paste0(path_synonyms,"Synonyms-Species.csv"))
  ind_to_rep <- which(animals_no_dup$class %in% synonyms_keywords$keyword) # indices in animals_no_dup$class that should be replaced
  ind_from_rep <- match(animals_no_dup$class, synonyms_keywords$keyword) # indices in synonyms to get replacements
  animals_no_dup$group <- rep(NA,nrow(animals_no_dup)) # generating new column with group classification
  animals_no_dup$group[ind_to_rep] <- synonyms_keywords$meaning[ind_from_rep[!is.na(ind_from_rep)]] # replacement words
  # There's an NA group; let's get rid of it
  animals_no_dup %>% filter(!is.na(group)) -> animals_no_dup
  
  # Now we spread this dataframe into one with one column per group
  animals_no_dup$identified <- rep(1,nrow(animals_no_dup))
  spread_animals <- animals_no_dup %>% 
    select(tsn,doi,group,identified) %>% 
    spread(key = group, value = identified)
  
  # We should get one column per paper, joining rows and getting info on groups in the same row
  spread_animals_summ <- spread_animals %>% 
    group_by(doi) %>% select(-tsn) %>%
    summarise_all(sum, na.rm=TRUE) # if this throws an error, change to summarise_all(list(~sum), na.rm=TRUE)
  
  
  # Two things left to do: add a human category, and cross with the papers dataset and add zero value rows for DOIs 
  # with no information on groups
  
  ## Working on humans now: we want all humans in one category, and subcategories of humans.
  # We'll start with the subcategories
  
  # First, filtering: there are some repeated information at the class level in the file
  df_dups_hum <- humans_data[c("term", "doi")]
  humans_no_dup <- humans_data[!duplicated(df_dups_hum), ]
  
  # Simplifying classes
  synonyms_keywords_humans <- read_csv(paste0(path_synonyms, "Synonyms-Humans.csv"))
  humans_no_dup$term_paste <- str_replace_all(humans_no_dup$term, "[[:punct:]]", "")
  ind_to_rep_hum <- which(humans_no_dup$term_paste %in% synonyms_keywords_humans$keyword) # indices in humans_no_dup$term_paste that should be replaced
  ind_from_rep_hum <- match(humans_no_dup$term_paste, synonyms_keywords_humans$keyword) # indices in synonyms to get replacements
  humans_no_dup$group <- rep(NA, nrow(humans_no_dup)) # generating new column with group classification
  humans_no_dup$group[ind_to_rep_hum] <- synonyms_keywords_humans$meaning[ind_from_rep_hum[!is.na(
    ind_from_rep_hum)]] # replacement words
  
  # Here we remove 'duplicates' in groups, so we keep only one row per group and doi 
  df_dups_group <- humans_no_dup[c("group", "doi")]
  humans_no_dup_group <- humans_no_dup[!duplicated(df_dups_group),]
  
  # Now we spread this dataframe into one with one column per group
  humans_no_dup_group$identified <- rep(1,nrow(humans_no_dup_group))
  spread_humans <- humans_no_dup_group %>% 
    select(doi,group,identified) %>% 
    spread(key = group, value = identified)
  
  # We should get one column per paper, joining rows and getting info on groups in the same row
  spread_humans_summ <- spread_humans %>% 
    group_by(doi) %>% 
    summarise_all(sum, na.rm=TRUE)
  
  # Now we create a new data frame that has doi and human columns. 
  # Actually, all of the DOIs that are there have human information, by definition of the file
  
  human_sum <- data.frame(doi = spread_humans_summ$doi, Humans=rep(1,nrow(spread_humans_summ)))
  
  # Making up a matrix about group info with all DOIs including those with no group info:
  
  # Start by reading the paper file and filtering on the time period:
  data_decade_summ <- data_decade %>% select(pubyear,doi)
  
  # then joining with animal groups
  matrix_anim <- data_decade_summ %>% 
    left_join(spread_animals_summ,by = c("doi" = "doi")) 
  
  # now we add humans
  matrix_groups <- matrix_anim %>% 
    left_join(human_sum,by = c("doi" = "doi")) 
  matrix_groups[is.na(matrix_groups)] <- 0 # replacing NAs by 0s in all columns
  
  # Now let's save this thing!
  save(object = matrix_groups, file = paste0(path.save.dict.results,"paper-term-dictionary-Taxonomy.RData"))
  
  # Computing some stats for animals
  papers_with_info_animals <- sum((matrix_groups %>%
                             select(-doi,-pubyear) %>%
                             rowSums()) > 0)
  group_names <- names(matrix_groups)[3:ncol(matrix_groups)]
  df_group_count <- by(t(matrix_groups[3:ncol(matrix_groups)]), group_names, function(ca) sum(colSums(ca) > 0))
  sink(paste0(path.save.stats, "dictionary-Taxonomy.txt"), append = FALSE)
  cat(paste0("Number of papers from which information was extracted: ", papers_with_info_animals))
  cat("\n")
  cat("\n")
  cat(paste0("Number of papers per category: "))
  print(df_group_count)
  cat("\n")
  sink()
  
  # OK, let's do the same procedure for the human categories
  matrix_human <- data_decade_summ %>% 
    left_join(spread_humans_summ,by = c("doi" = "doi")) 
  matrix_human[is.na(matrix_human)] <- 0 # replacing NAs by 0s in all columns
  
  save(object = matrix_human, file = paste0(path.save.dict.results,"paper-term-dictionary-Humans.RData"))
  
  # Computing some stats for humans
  papers_with_info_humans <- sum((matrix_human %>%
                                     select(-doi,-pubyear) %>%
                                     rowSums()) > 0)
  cat_names <- names(matrix_human)[3:ncol(matrix_human)]
  df_cat_count <- by(t(matrix_human[3:ncol(matrix_human)]), cat_names, function(ca) sum(colSums(ca) > 0))
  sink(paste0(path.save.stats, "dictionary-Humans.txt"), append = FALSE)
  cat(paste0("Number of papers from which information was extracted: ", papers_with_info_humans))
  cat("\n")
  cat("\n")
  cat(paste0("Number of papers per category: "))
  print(df_cat_count)
  cat("\n")
  sink()
  
}
