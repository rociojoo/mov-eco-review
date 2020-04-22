#########################################
# "Making word cloud graphs with ggplot"
#########################################
# This will make wordcloud graphs using ggworldcloud. In most cases it will work for any
# Other input youd like with some minor modifications

###  Read in libraries and arguments
# Arguments are as follows : 
#   **path_data** : path for cleaned WoS reference csv  
#   **path.plots** : folder path to save plots  
#   **path_topics** : folder of topics rds outputs  
#   **data** : df with words (as term column) and frequencies (as beta column) by topic (as topic column)  
#   **values_tresh** : lower limit for frequencies of words  
#   **indiv_angle** : TRUE if each word would have a different angle which could be (-90, -45, 0, 45, 90). 
#   FALSE if every word is horizontal  
#   **max_size_area** : to plot each wordcloud. Depends on number of words. If too large, there could be a lot of
#   space between wordclouds. If too small, some words may not appear in the wordcloud  
#   **height_file_par** and **width_file_par** : height and width of file where we save the plots (in inches)
#   filename : for the pdf with wordclouds  
#   **eccentricity_par** : proportion of horizontal display of wordcloud (respect to vertical). Default 1 (sphere or square like shape).  
#   **rm_outside_par** : removes text that would not be fitted. TRUE to avoid overlap. But try not to lose text. Default FALSE. 

# libraries
library(tidyverse)
library(topicmodels)
library(ggwordcloud)

# paths
path_data <- "./Data/ProcessedQueries/References/cleaned_papers_all_years_simple.csv"
path.plots <- "./Rocio/Plots/"
path_topics <- "./Data/Topics/"

# arguments
indiv_angle = FALSE
height_file_par = 26
width_file_par = 26
max_size_area = 30 
values_thresh = 0.003
rm_outside_par = FALSE
eccentricity_par = 1

### Now read in data and prepare it

# main dataset

papers <- read.csv(file = paste0(),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

data_decade_summ <- data_decade %>% select(doi,pubyear)

# LOAD LDA data
N_topics <- 15
alpha_par <- 1 #NULL
method_par <- "VEM" # logliks are way higher with VEM

if (is.null(alpha_par)){
  modk <- readRDS(file = paste0(path_topics,"BestTopicModel",N_topics,".rds"))
}else if(method_par == "VEM"){
  modk <- readRDS(file = paste0(path_topics,"NewBestTopicModel",N_topics,"_alpha_",alpha_par,"_method_",method_par,"_filtered_II.rds"))
}else{
  modk <- readRDS(file = paste0(path_topics,"BestTopicModel",N_topics,"_alpha_",alpha_par,".rds"))
}

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

papers_gamma$topic_lab <- plyr::mapvalues(papers_gamma$topic, from = t(as.data.frame(new_order)), to = new_label_for_order)

##################################
# Create and save the word clouds
##################################

# 1. Topic as a mixture of words
# beta: the probability of that term being generated from that topic.
papers_beta <- tidytext::tidy(modk, matrix = "beta")
# papers_beta
head(papers_beta)

papers_beta$topic_lab <- as.factor(papers_beta$topic)
papers_beta$topic_lab <- plyr::mapvalues(papers_beta$topic_lab, from = t(as.data.frame(new_order)), to = new_label_for_order)
papers_beta$topic_lab <- factor(papers_beta$topic_lab, levels = new_label_for_order)

topic_sample <- data %>% filter(beta > values_thresh) #%>%  select(term,beta)

if (indiv_angle == TRUE){
  table_topic <- topic_sample %>%
    mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 2, 4, 2, 1)))
}else{
  table_topic <- topic_sample %>%
    mutate(angle = rep(0,nrow(topic_sample)))
}

plot_w <- ggplot(table_topic, aes(label = term, size = beta, angle = angle, 
  color = beta)) +
  geom_text_wordcloud_area(rm_outside = rm_outside_par, eccentricity = eccentricity_par,grid_margin = 0) + #area_corr_power = 1,
  scale_size_area(max_size = max_size_area) +
  theme_bw() + scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1))+
  facet_wrap(~topic_lab, scales = "free",shrink = F) +
  theme(strip.text = element_text(size = 45),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    strip.background = element_rect(fill = 'white')) 
