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
path_topics <- "./Data/Topics/"

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

topic_labels <- c("Social interactions and dispersal","Movement models","Habitat selection","Detection and data","Home ranges",
                  "Aquatic systems","Foraging in marine megafauna","Biomechanics","Acoustic telemetry",
                  "Experimental designs","Activity budgets","Avian migration","Sports","Human activity patterns","Breeding ecology")

# Now choose dictionary
dictionary <- "Taxonomy" # "Humans"
res <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1, 
  paper_cat_out=TRUE, filter_lines=TRUE, suffix=NULL, 
  path_processed_dictionaries)

newtaxa_df <- res[[2]]
rownames(newtaxa_df) <- NULL

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
plot_df <- merge(plot_df,data.frame(Topic = 1:15, topic_labels), by='Topic')


# Color ramp
color_pallete <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')


# # Reorder if youd like
plot_df$Taxon <-  factor(plot_df$Taxon,
                         levels = names(sort(tapply(plot_df$Total,plot_df$Taxon, sum),decreasing=T)))
topic_labels
plot_df$topic_labels <- factor(plot_df$topic_labels, levels = topic_labels)

# make a dataframe of the n's for each group to add later
ann_text <- plot_df %>% group_by(topic_labels) %>% 
  summarise(n = sum(Total)) %>% 
  mutate(Taxon = 'Amphibians', total_prop = 0.8)

ggplot(data=plot_df, aes(x=Taxon, y=total_prop, fill=Taxon)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = color_pallete) +
  facet_wrap(facets = vars(topic_labels)) +
  geom_text(data = ann_text,label = paste0('(n = ',ann_text$n,')')) +
  theme_bw() + 
  theme(axis.title.y = element_text(size=16),
        axis.text.y = element_text(size=12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 13)) + 
  ylab('Total proportion of papers within a topic') 

ggsave(filename=paste0(path.plots,"Barplots_topics_taxa.png"), height=10,width=14)
# ggsave('Manuscript/Images/Barplots_topics_taxa1.png', width=12,height=10)
