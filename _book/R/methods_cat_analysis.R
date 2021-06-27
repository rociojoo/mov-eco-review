methods_cat_analysis <- function(matrix_CatTerm, synonyms_keywords, col_cat = 3, data_decade){
  # matrix_CatTerm is the matrix with the papers as rows and the terms as columns
  # synonyms_keywords is the dataframe with the new dictionary (meanings)
  # col_cat is the number of meaning column 
  # data_decade is the data frame with summary information per paper
  
  # Internally, I'm filtering out tests and other stuff:
  filter_out_methods <- unique(c(which(synonyms_keywords$meaning2 == "test" | 
                                         synonyms_keywords$meaning2 == "model selection" | 
                                         synonyms_keywords$meaning2 == "simulation" |
                                         is.na(synonyms_keywords$meaning2) == TRUE |
                                         synonyms_keywords$meaning3 == "other"),
                                 grep("likelihood",synonyms_keywords$keyword)))
  synonyms_keywords <- as.data.frame(synonyms_keywords[-filter_out_methods,])
  
  
  ext_subcat <- unique(synonyms_keywords[ , col_cat + 1])
  
  matrix_Ext_SubCat <- matrix(0,ncol=length(ext_subcat),nrow=dim(matrix_CatTerm))
  colnames(matrix_Ext_SubCat) <- ext_subcat
  rownames(matrix_Ext_SubCat) <- rownames(matrix_CatTerm)
  
  for (i in 1:length(ext_subcat)){ # for each subcategory
    terms_ext <- synonyms_keywords$keyword[which(synonyms_keywords[ , col_cat + 1] == ext_subcat[i])] # which keywords correspond to the subcategory
    col_matrix <- which(colnames(matrix_CatTerm) %in% terms_ext) # which columns in matrix_CatTerm does that represent
    # now check with rows have at least one "1" among those categories
    # first sum by row. it should be >= 1
    ind_matrix <- which(apply(matrix_CatTerm[,col_matrix],1,sum) > 0)
    matrix_Ext_SubCat[ind_matrix,i] <- 1
  }
  
  
  total_ext <- sum(rowSums(matrix_Ext_SubCat)>0)
  
  table_sub <- sort(round(apply(matrix_Ext_SubCat,2,sum)/total_ext*100,1),decreasing = TRUE)
  
  # Now, join with year
  
  df_Ext_SubCat <- as.data.frame(matrix_Ext_SubCat) %>% 
    mutate(doi = rownames(matrix_Ext_SubCat))
  
  joined_df <- df_Ext_SubCat %>% left_join(data_decade, by = "doi") %>% select(ext_subcat,pubyear)
  
  # And counting by category by year
  
  joined_df_year <- joined_df %>% group_by(pubyear) %>% summarise_all(sum)
  
  joined_df_nozero <- joined_df[apply(joined_df[,ext_subcat],1,sum) > 0,]
  
  sum_year <- joined_df_nozero %>% group_by(pubyear) %>% tally()
  
  joined_df_prop_year <- joined_df_year[,ext_subcat]/matrix(rep(sum_year$n,each=length(ext_subcat)),ncol=length(ext_subcat),byrow=TRUE)
  
  joined_df_prop_year <- joined_df_prop_year %>% mutate(year = joined_df_year$pubyear) 
  
  df_plot_prop <- joined_df_prop_year %>% gather(key = subcategory, value = prop_papers, -year)
  
  
  values_prop <- sort(unique(df_plot_prop$prop_papers))
  values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.1)
  values_year <- seq(from=min(df_plot_prop$year), to=max(df_plot_prop$year),by=1)
  
  color.pallete<-brewer.pal(length(ext_subcat), "Paired") # color palette
  plot_subts <- ggplot(data  = df_plot_prop, mapping = aes(x = year, y = prop_papers, color = subcategory)) +
    geom_line(size=1.5) +
    scale_colour_manual(name="category", values = color.pallete) + #breaks = corrected_df$col,
    # labels=corrected_df$category) +
    scale_x_continuous(breaks = values_year) +
    scale_y_continuous(breaks = values_breaks) +
    xlab("") + ylab("Proportion of articles in a year") +
    theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
          legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=16),
          axis.title.y = element_text(margin = margin(r=10)), 
          axis.title.x = element_text(margin = margin(t=10)))
  
  ## Now, let's sum up everything that is not general
  ## 
  col_others <- which(!(colnames(matrix_Ext_SubCat) %in% "generic"))
  others_sum <- rowSums(matrix_Ext_SubCat[,col_others])
  others_sum[others_sum > 1] <- 1
  gen_others_df <- cbind.data.frame(generic = matrix_Ext_SubCat[,which(colnames(matrix_Ext_SubCat) %in% "generic")],
                   others = others_sum)
  
  total_gen_others <- sum(rowSums(gen_others_df)>0)
  
  table_gen_others <- sort(round(apply(gen_others_df,2,sum)/total_gen_others*100,1),decreasing = TRUE)
  
  # Now, join with year
  
  df_gen_others_SubCat <- as.data.frame(gen_others_df) %>% 
    mutate(doi = rownames(gen_others_df))
  
  joined_df_gen_others <- df_gen_others_SubCat %>% left_join(data_decade, by = "doi") %>% select(generic,others,pubyear)
  
  # And counting by category by year
  
  joined_df_year_go <- joined_df_gen_others %>% group_by(pubyear) %>% summarise_all(sum)
  
  joined_df_nozero_go <- joined_df_gen_others[apply(joined_df_gen_others[,c("generic","others")],1,sum) > 0,]
  
  sum_year_go <- joined_df_nozero_go %>% group_by(pubyear) %>% tally()
  
  joined_df_prop_year_go <- joined_df_year_go[,c("generic","others")]/matrix(rep(sum_year_go$n,each=2),ncol=2,byrow=TRUE)
  
  joined_df_prop_year_go <- joined_df_prop_year_go %>% mutate(year = joined_df_year_go$pubyear) 
  
  # correction
  joined_df_prop_year_go$generic <- 1- joined_df_prop_year_go$others
  
  df_plot_prop_go <- joined_df_prop_year_go %>% gather(key = subcategory, value = prop_papers, -year)
  
  values_prop <- sort(unique(df_plot_prop_go$prop_papers))
  values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.1)
  values_year <- seq(from=min(df_plot_prop_go$year), to=max(df_plot_prop_go$year),by=1)
  
  color.pallete <- c("#a6cee3", "#fdbf6f") # color palette
  plot_subts_go <- ggplot(data  = df_plot_prop_go, mapping = aes(x = year, y = prop_papers, color = subcategory)) +
    geom_line(size=1.5) +
    scale_colour_manual(name="category", values = color.pallete) + #breaks = corrected_df$col,
    # labels=corrected_df$category) +
    scale_x_continuous(breaks = values_year) +
    scale_y_continuous(breaks = values_breaks) +
    xlab("") + ylab("Proportion of articles in a year") +
    theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
          legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=16),
          axis.title.y = element_text(margin = margin(r=10)), 
          axis.title.x = element_text(margin = margin(t=10)))
  
  
  table_gen_others <- c(table_gen_others, onlygen = 
                          round(sum(gen_others_df$generic == 1 & gen_others_df$others == 0)/total_gen_others*100,1))
  
  
  return(list(table_sub,plot_subts,table_gen_others,plot_subts_go))
  
}
