library(tidyverse)
library(cowplot)
library(paletteer)

# Arguments
path <- "./Data/"
path_processed_dictionaries <- "./Data/Dictionary/Papers-Term/"
path_dictionary_info <- "./Data/Dictionary/"

source("R/methods_cat_analysis.R")

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)


dictionary <- "Methods"

load(paste0(path_processed_dictionaries,"paper-term-dictionary-",dictionary,".RData"))
# both matrices have the keywords ordered alphabetically:

total_useful_papers <- apply(matrix_CatTerm[2:ncol(matrix_CatTerm)],1,sum)
matrix_CatTerm <- matrix_CatTerm[total_useful_papers>0,]

keywords <- unlist(lapply(strsplit(colnames(matrix_CatTerm), " : ")[-1],"[[",2))
colnames(matrix_CatTerm) <- c("doi",keywords)
rownames(matrix_CatTerm) <- matrix_CatTerm$doi

matrix_CatTerm <-
  matrix_CatTerm[,-1] %>%
  select(sort(colnames(.))) # sort columns by alphabetical order

synonyms_keywords <- read_csv(paste0(path_dictionary_info,"Synonyms-Methods.csv"))

res <- methods_cat_analysis(matrix_CatTerm = matrix_CatTerm, synonyms_keywords, col_cat = 3, data_decade)
## print(res[[1]]) gives the output of the Table "Percentage of papers using each type of statistical method."

# Internally, I'm filtering out tests and other stuff:
filter_out_methods <- unique(c(which(synonyms_keywords$meaning2 == "test" | 
    synonyms_keywords$meaning2 == "model selection" | 
    synonyms_keywords$meaning2 == "simulation" |
    is.na(synonyms_keywords$meaning2) == TRUE |
    synonyms_keywords$meaning3 == "other"),
  grep("likelihood",synonyms_keywords$keyword)))
synonyms_keywords <- as.data.frame(synonyms_keywords[-filter_out_methods,])

col_cat=3
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

plot_df <- df_plot_prop
head(plot_df)
plot_df$subcategory <- rep(c('Generic','Spatial','Movement','TimeSeries','Social','Spatiotemporal'), each = 10)

values_prop <- sort(unique(plot_df$prop_papers))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.1)
values_year <- seq(from=min(plot_df$year), to=max(plot_df$year),by=1)

methods_colors <- paletteer_d("colorblindr::OkabeIto") %>%
  as.vector()
names(methods_colors) <- snakecase::to_upper_camel_case(names(table_sub))
methods_color_palette_current <- methods_colors[unique(plot_df$subcategory)]

ggplot(data  = plot_df, mapping = aes(x = year, y = prop_papers, color = subcategory, fill = subcategory)) +
  geom_line(size=1.5, linetype = 3) +
  geom_point(size = 5.5) +
  scale_color_manual(name="Method",values = methods_color_palette_current) +
  scale_fill_manual(name="Method",values = methods_color_palette_current) +
  scale_x_continuous(breaks = values_year) +
  scale_y_continuous(breaks = values_breaks) +
  theme_bw()+xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(r=10),size=17), 
        axis.title.x = element_text(margin = margin(t=10)),
        legend.key.size = unit(2,"line"),
        legend.title=element_text(size=16))
