library(tidyverse)

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

res <- methods_cat_analysis(matrix_CatTerm = matrix_CatTerm, synonyms_keywords, col_cat = 3)


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


values_prop <- sort(unique(df_plot_prop$prop_papers))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.1)
values_year <- seq(from=min(df_plot_prop$year), to=max(df_plot_prop$year),by=1)

color.pallete<-brewer.pal(length(ext_subcat), "Paired") # color palette

plot_df <- df_plot_prop
head(plot_df)
plot_df$subcategory <- rep(c('Generic','Spatial','Movement','Time-series','Social','Spatial-temporal'), each = 10)
# Run a quick linear model to measure which trend lines are positive or negative
# we'll reference this when we choose our colors
here <- by(plot_df, plot_df$subcategory, function(x)
  lm(x$prop_papers ~ x$year)$coefficients[2]
)
plot_df$subcategory <- factor(plot_df$subcategory, levels= names(sort(here)))
# Create a grouping variable based on this value

grouping <- data.frame(subcategory = c(names(here)[here<=0.003 & here>=(-0.003)],names(here)[here<(-0.003)],names(here)[here>0.003]))
grouping$group <- seq_along(grouping$subcategory)

plot_df <- merge(plot_df,grouping, by='subcategory')

# Now to make our aesthetic features which will be added with scale_*_manual()
# Colors
# Make a color ramp where the amount of 'grays' will determine the highlighted categories
# colfunc <- colorRampPalette(c("red",'pink','gray','gray','gray',"blue"))
# colorz <- colfunc(nrow(here))
# names(colorz) <- names(sort(here))
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

#Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
colorz <- Tol_muted[1:(length(here)) %% (length(Tol_muted))]
# change problematic colors to gray60
# gray 60 "#7f7f7f"
# black "#000000"
names(colorz) <- names(sort(here))
colorz
colorz[6] <- "#7f7f7f"
# line types
# just need to spread linetypes out enough so that the color and alpha can help distinguish as well
# manual
# manual 5 = dash, 3 = dotted, 1 = solid
linetypez <- c(5,3,3,3,3,1)
# or random
# linetypez <- rep(1:6,times=ceiling(length(levels(plot_df$Topic))/6))
# linetypez <- linetypez[seq_along(levels(plot_df$Topic))]
names(linetypez) <- names(sort(here))

# alpha
# Changing alpha will help to make the important categories pop.
# Create a gradient of alphas from 1 -> .2 -> so none trend lines are grayed out.
nz <- length(here)
# automatically
#alphaz <- c((1*nz/2):(.2*nz/2)/nz*2,(.2*nz/2):(1*nz/2)/nz*2,ifelse(nz%%2==0,NULL,1))
# or manually

alphaz <- c(1,.7,.4,.4,.4,1)
names(alphaz) <- names(sort(here))

# line width
sizez <-  c(2,1,1,1,1,2)
#names(sizez) <- names(here)
sizez
sizez <- rep(sizez, each = 10)
sizez
# You have to include color, linetype, and alpha in the mapping even if youre going to override it anyway.

p <- ggplot(
  data  = plot_df) +
  geom_line(size=1.5, 
    mapping = aes(x = year, y = prop_papers, color = subcategory, group = group, linetype = subcategory, alpha = subcategory)
  ) +
  scale_color_manual(name='Methods',values = colorz) +
  scale_linetype_manual(name='Methods',values = linetypez) +
  scale_alpha_manual(name='Methods',values = alphaz)+
  theme_classic()+xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
    legend.position = "none", legend.justification = "right",legend.text=element_text(size=15),
    axis.title.y = element_text(margin = margin(r=10),size=17), 
    axis.title.x = element_text(margin = margin(t=10)),
    legend.key.size = unit(2,"line"),
    legend.title=element_text(size=16))

start_pos <- plot_df %>% group_by(subcategory) %>% summarise(y = last(prop_papers)) %>% mutate(x = 2018)
start_pos$colorz <- colorz
start_pos

start_pos$x_new <- start_pos$x + 0.1
start_pos$y_new <- start_pos$y + c(0,0,0,0,0,0)
p + geom_text(data = start_pos, aes(x =x_new ,y=y_new, label = subcategory), color=colorz,hjust=0,size=5)+
  coord_cartesian(xlim = c(2009, 2018),clip = 'off') + 
  theme(plot.margin = unit(c(1,10,1,1), "lines"))

ggsave("Manuscript/Images/method_ts1.png", width=12,height=8)
