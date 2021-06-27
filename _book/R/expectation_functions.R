expectation_functions <- function(dictionary, data_all, num_cat=NULL, ini_cat=1, paper_cat_out=FALSE, 
                                  filter_lines=FALSE, suffix=NULL, path_processed_dictionaries){
  # dictionary: type of dictionary (e.g. Data, Software)
  # data_all: papers data frame
  # num_cat: pre-defined number of categories to show stats from (the most frequent)
  # ini_cat: category that should be ploted first (it's numeric, a rank based on popularity)
  #   by default, it will be category that was the most mentioned by the papers
  # paper_cat_out: TRUE if one of the outputs of this function is the matrix of 1s and 0s with no 0s
  # rows;  by default it's FALSE
  # filter_lines: if for some reason the number of rows in the dictionary matrix does not match the number
  #   of papers in the data frame, to do a filtering based on doi; it would be weird to need this to be TRUE now
  # suffix: if the name of the file with the dictionary matrix has a special suffix (e.g. with the time period)
  #   this should be specified
  # path_processed_dictionaries: path of where the dictionary matrices are
  
  if (is.null(suffix)){
    load(paste0(path_processed_dictionaries,"paper-term-dictionary-",dictionary,".RData"))
  }else{
    load(paste0(path_processed_dictionaries,"paper-term-dictionary-",dictionary,"-",suffix,".RData"))
  }
  
  if (dictionary == "Taxonomy"){
    matrix_CatTerm <- matrix_groups %>% select(-pubyear)
  }else if(dictionary == "Humans"){
    matrix_CatTerm <- matrix_human %>% select(-pubyear)
  }
  
  if (filter_lines == TRUE){
    matrix_CatTerm <- matrix_CatTerm[which(matrix_CatTerm$doi %in% data_all$doi),]
  }
  
  
  ## Counts for each term
  term_counts <- colSums(matrix_CatTerm[, -1])
  ## Categories of terms
  category <- unlist(lapply(strsplit(colnames(matrix_CatTerm), " : "), "[[", 1))[-1]
  ## Cumulative count per category (= stats above)
  list_category <- as.character(unique(category))
  paper_cat <- as.data.frame(sapply(1:length(list_category),function(x){
    ind <- grep(list_category[x],category)
    cat_tot <- apply(as.matrix(matrix_CatTerm[,ind+1]),1,sum)
    cat_tot[cat_tot>1] <- 1
    return(cat_tot)
  })) # one column for topic and one row per paper (data frame of 1s and 0s)
  rownames(paper_cat) <- matrix_CatTerm$doi
  colnames(paper_cat) <- list_category 
  table_cat <- data.frame(num_papers=apply(paper_cat,2,sum))
  table_cat$cat <- list_category
  table_cat <- table_cat %>% 
    arrange(desc(num_papers)) %>% 
    select(sort(colnames(.))) %>% 
    filter(num_papers>0)
  # Now taking the information about year of each paper
  # But I am only going to care about the papers that actually have a topic (there are papers without identified topic)
  total_useful_papers <- apply(paper_cat,1,sum)
  
  if (dictionary != "Taxonomy" && dictionary != "Humans"){
    
    paper_cat <- paper_cat[total_useful_papers>0,]
    
    
    paper_cat_year <- dplyr::left_join(paper_cat %>%
                                         mutate(doi=rownames(paper_cat)),
                                       data_all,
                                       by="doi") # join both data frames based on doi
  }else{
    # Now taking the information about year of each paper
    # But I am only going to care about the papers that actually have a topic (there are papers without identified topic)
    total_useful_papers <- apply(paper_cat,1,sum)
    paper_cat_year <- cbind.data.frame(paper_cat,pubyear=matrix_groups$pubyear,doi=matrix_groups$doi)
    paper_cat_year <- paper_cat_year[total_useful_papers > 0, ]
    paper_cat <- paper_cat_year[total_useful_papers > 0, ]
  }
  
  counts_cat_year <- as.data.frame(sapply(1:length(list_category),function(x){
    tapply(paper_cat_year[,x], paper_cat_year$pubyear, FUN=sum)
  }))
  colnames(counts_cat_year) <- list_category   
  
  
  # number of categories to plot
  if (is.null(num_cat)){
    num_cat <- min(dim(table_cat)[1],12)
  }else if(num_cat > 12){
    print("you have more than 12 categories; we will only consider the first 12")
    num_cat <- 12
  }
  # initial category to plot (sorted by frequency, decreasing)
  if (ini_cat > 1){
    num_cat <- num_cat + ini_cat - 1
  }
  
  counts_cat_year <- counts_cat_year[,table_cat$cat[ini_cat:num_cat]]
  counts_cat_year$year <- as.numeric(rownames(counts_cat_year)) # got table of counts of papers for each category per year
  cat_year_df <- counts_cat_year %>% gather(key=category,value=num_papers,-year) # gather as a df that ggplot would like
  
  papers_year <- paper_cat_year[,c("doi","pubyear")]
  papers_year$pubyear <- as.factor(papers_year$pubyear)
  papers_year <- papers_year %>% 
    group_by(pubyear) %>% 
    tally()
  colnames(papers_year) <- c("year","total")
  papers_year$year <- as.numeric(as.character(papers_year$year))
  
  corrected_df <- left_join(cat_year_df,papers_year,by="year")
  corrected_df <- corrected_df %>% 
    mutate(prop_papers = num_papers/total)
  
  if (paper_cat_out == TRUE){
    return(list(corrected_df,paper_cat))
  }else{
    return(corrected_df)
  }
}