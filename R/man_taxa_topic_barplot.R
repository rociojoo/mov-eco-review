########################
# Making taxa bar plots
########################

library(tidyverse)
# library(topicmodels) #install in ubuntu libgsl-dev and gsl-bin
source("./R/expectation_functions.R")

# Arguments
path <- "./Data/ProcessedQueries/References/"
path.plots <- "./Rocio/Plots/"

# Summarizing
if (!dir.exists(path.plots)){
  dir.create(path.plots)
}
path_processed_dictionaries <- "./Data/Dictionary/Papers-Term/"
path_dictionary_info <- "./Data/Dictionary/"

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

# Now choose topic
dictionary <- "Taxonomy" # "Humans"
res <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1, 
  paper_cat_out=TRUE, filter_lines=TRUE, suffix=NULL, 
  path_processed_dictionaries)

newtaxa_df <- res[[2]]
rownames(newtaxa_df) <- NULL

path_topics <- "./Data/Topics/"
papers_summ <- readRDS(paste0(path_topics,"Topics_Paper.rds"))

taxa_topics <- left_join(newtaxa_df,papers_summ[, c("doi","top_50")],by="doi")

taxa_topics_50 <- taxa_topics[which(is.na(taxa_topics$top_50) == FALSE),]

taxa_topics_50_df <- 
  taxa_topics_50 %>% 
  gather(key=taxon, value=presence, -pubyear, -doi, -top_50)

taxa_topics_50_df_presence <- 
  taxa_topics_50_df %>% 
  filter(presence == 1)

taxa_topics_50_count <- taxa_topics_50_df_presence %>% 
  group_by(taxon,top_50) %>% 
  count()

names(taxa_topics_50_count) <- c("Taxon","Topic","Total")

# Begin plotting
plot_df <- taxa_topics_50_count
plot_df <- merge(plot_df,data.frame(total_topic = tapply(plot_df$Total, plot_df$Topic,sum )), by.x='Topic', by.y = 0)
plot_df$total_prop <- plot_df$Total/plot_df$total_topic

# Color ramp
color_ramp5 <- colorRampPalette(c( 'darkolivegreen4','wheat2','darkorchid3'))(length(unique(plot_df$Taxon)))

# # Reorder if youd like
# plot_df$Taxon <- factor(plot_df$Taxon, 
#   levels = c('Amphibians', 'Reptiles', 'Birds',
#   'Humans','Mammals','Fish','Crustaceans',
#   'Mollusks','Insects','others') )

ggplot(data=plot_df, aes(x=Taxon, y=total_prop, fill=Taxon)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = color_ramp5)+
  facet_wrap(facets = vars(Topic)) +
  theme_bw() + 
  theme(axis.title.y = element_text(size=16),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()) + ylab('Total proportion of papers within a topic')
  

