################################
# Taxonomy Text Mining Functions
################################

# text searching
search_function <-  function(data, ids=NULL, terms = TRUE){
  
  out_list <- lapply(data, function(i){
  t <- grep(i, titles)
  k <- grep(i, keywords)
  a <- grep(i, abstracts)
  t<- if (length(t)>0) t else NA 
  k<- if (length(k)>0) k else NA
  a<- if (length(a)>0) a else NA
  num <- which(i==data)
  if (is.null(ids)) { ids <- seq_along(data)}
  if(ceiling(num/100)==floor(num/100) & num>100 ) cat('\n',num,'\n')
  return(list(id=ids[num],term=i,t=t,k=k,a=a))
  })
  
  good_df <- out_list[!sapply(out_list, function(x) all(is.na(unlist(x[2:4]))))]
  t_df <- do.call(rbind,lapply(good_df, function(x) data.frame(id = x$id, term = x$term, paper_id = x$t)))
  k_df <- do.call(rbind,lapply(good_df, function(x) data.frame(id = x$id, term = x$term, paper_id = x$k)))
  a_df <- do.call(rbind,lapply(good_df, function(x) data.frame(id = x$id, term = x$term, paper_id = x$a)))
  t_df$section<-'title'
  k_df$section<-'keyword'
  a_df$section<-'abstract'
  
  all_df <- bind_rows(list(t_df, k_df, a_df))
  if (!terms) all_df <- subset(all_df, select=-term)
  
  return(all_df)
}

# Function to get Class/order given higher-level taxonomic unit
getClassOrder <- function(sp) {
  y <- NA
  x <- tax_units %>%
    filter(name_usage=="valid") %>%
    select(complete_name, tsn, parent_tsn, rank_id) %>%
    filter(tsn==sp) %>%
    collect()
  i <- x$rank_id
  if (nrow(x)==0) {
    stop("This name is not in the database.")
  }
  if(i==100){y <- x$complete_name}
  while (i>60) {
    x<- subset(tax_units, tsn==x$parent_tsn, select=c(complete_name, tsn, parent_tsn, rank_id))
    #x <- get_parent(x)
    i <- x$rank_id
    if(i==100){y <- x$complete_name}
  }
  return(data.frame(tsn=sp,class=x$complete_name,order=y))
}
