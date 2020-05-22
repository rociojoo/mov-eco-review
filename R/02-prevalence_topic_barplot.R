
library(plyr)
library(dplyr) 
library(tidyr) # spread
library(ggplot2)
library(readr)
library(tidytext) 
library(magrittr) 
library(topicmodels)
library(cowplot)
library(viridis)
library(RColorBrewer)
library(stringr) # to use str_detect for numbers

# paths
path_data <- "./Data/Rocio-temporal/" # path to main dataset
path.plots <- "./Rocio/Plots/" # path for storing plots
path_topics <- "./Data/Topics/" # path to topic files

# main dataset

papers <- read.csv(file = paste0(path_data,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

data_decade_summ <- data_decade %>% select(doi,pubyear)

# LOAD LDA data
N_topics <- 15
alpha_par <- 1 #NULL
method_par <- "VEM" # logliks are way higher with VEM

modk <- readRDS(file = paste0(path_topics,"NewBestTopicModel",N_topics,"_alpha_",alpha_par,"_method_",method_par,"_filtered_II.rds"))

############ NUMBER OF PAPERS RELATED TO EACH TOPIC #################

papers_gamma <- tidytext::tidy(modk, matrix = "gamma")
head(papers_gamma)

# Each of these values is an estimated proportion of words from that document that are generated from that topic.
# For example, the model estimates that only about 12.3% of the words in document 2 were generated from topic 1.

# strength of total connection of topics to papers
gamma_topic <- papers_gamma %>% group_by(topic) %>% dplyr::summarise(gamma = sum(gamma)) %>% arrange(desc(gamma))
gamma_topic$percentage <- gamma_topic$gamma/length(unique(papers_gamma$document))*100
new_order <-  gamma_topic %>% select(topic)
new_label_for_order <- 1:N_topics

papers_gamma$topic_lab <- mapvalues(papers_gamma$topic, from = t(as.data.frame(new_order)), to = new_label_for_order)

# Long format to short format
papers_gamma_onlylab <- select(papers_gamma,-topic) # only reordered labels
papers_gamma_short <- spread(papers_gamma_onlylab, key = topic_lab, value = gamma)
names(papers_gamma_short) <- c("document", paste("Topic", names(papers_gamma_short)[-1])) 

topic_labels <- c("Social interactions and dispersal","Movement models","Habitat selection","Detection and data","Home ranges",
                      "Aquatic systems","Foraging in marine megafauna","Biomechanics","Acoustic telemetry",
                      "Experimental designs","Activity budgets","Avian migration","Sports","Human activity patterns","Breeding ecology")


################ CALCULATE AN IMPORTANCE MEASURE FOR THE TOPICS (SUM OF GAMMAS) ###################
# Idea: I take the gamma matrix and then sum up for each column, then table and barplot
# 
gamma_sum <- apply(papers_gamma_short[,-1],2,sum)
gamma_sum_df <- data.frame(Topic = topic_labels, Gamma = gamma_sum)
# gamma_sum_df <- data.frame(Topic = 1:15, Gamma = gamma_sum)
gamma_sum_ordered <- gamma_sum_df[order(gamma_sum_df$Gamma, decreasing = FALSE), ]
gamma_sum_ordered$Topic <- factor(gamma_sum_ordered$Topic, levels = gamma_sum_ordered$Topic)


ggplot(gamma_sum_ordered, aes(x=Topic,y=Gamma)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  xlab("Topic") + ylab("Total gamma") +
  theme_cowplot() + theme(text = element_text(size=20),axis.text.y = element_text(size=16))
# ggsave(filename=paste0(path.plots,"SumGammas_labels.pdf"), height=7,width=7)
# ggsave(filename=paste0(path.plots,"SumGammas_labels.png"), height=7,width=7)
