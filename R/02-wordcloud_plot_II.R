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
#   **path_dict_tools** : folder of auxiliary functions for text mining
#   **data** : df with words (as term column) and frequencies (as beta column) by topic (as topic column)  
#   **values_tresh** : lower limit for frequencies of words  
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
library(tidytext) # to use unnest_tokens and tidy
library(textstem) # to use lemmatize_words
library(tm) # to use stopwords
library(stringr) # to use str_detect

# paths
path_data <- "./Data/Rocio-temporal/cleaned_papers_all_years_simple.csv"
path.plots <- "./Rocio/Plots/"
path_topics <- "./Data/Topics/"
path_dict_tools <- "./Data/Dictionary/AuxiliaryTextMining/"

# calling auxiliary functions
source("./R/Americanizing.R")
source("./R/cleaning_words_abstract.R")

# arguments
height_file_par = 30
width_file_par = 30
max_size_area = 30 
values_thresh = 0.003
rm_outside_par = FALSE
eccentricity_par = 1
filename <- paste0("wordcloud_II.pdf")

### Now read in data and prepare it

# main dataset

papers <- read.csv(file = paste0(path_data),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

data_decade_summ <- data_decade %>% select(doi,pubyear)

# LOAD LDA data
N_topics <- 15
alpha_par <- 1 #NULL
method_par <- "VEM" 

modk <- readRDS(file = paste0(path_topics,"NewBestTopicModel",N_topics,"_alpha_",alpha_par,"_method_",method_par,"_filtered_II.rds"))

############ NUMBER OF PAPERS RELATED TO EACH TOPIC #################

# this "gamma" part serves only to get the prevalence of papers and rank them,
# so that we use this ranking to order the wordclouds

papers_gamma <- tidytext::tidy(modk, matrix = "gamma")
head(papers_gamma)

# Each of these values is an estimated proportion of words from that document that are generated from that topic.
# For example, the model estimates that only about 12.3% of the words in document 2 were generated from topic 1.

# strength of total connection of topics to papers
gamma_topic <- papers_gamma %>% group_by(topic) %>% dplyr::summarise(gamma = sum(gamma)) %>% arrange(desc(gamma))
gamma_topic$percentage <- gamma_topic$gamma/length(unique(papers_gamma$document))*100
new_order <-  gamma_topic %>% select(topic)
new_label_for_order <- 1:N_topics
#
papers_gamma$topic_lab <- plyr::mapvalues(papers_gamma$topic, from = t(as.data.frame(new_order)), to = new_label_for_order)

papers_gamma_onlylab <- select(papers_gamma,-topic) # only reordered labels
papers_gamma_short <- spread(papers_gamma_onlylab, key = topic_lab, value = gamma)

##############################################################
# Create and save the word clouds of abstracts of each topic #
##############################################################

## Step 1: select papers that have > 0.75 gamma for a topic
## Step 2: join with abstracts
## Step 3: write a function to: for each set of abstracts (topic), 
## process words and compute frequencies
## the thing is that, word frequency in a topic is related to the number of abstracts related to the topic
## and we will compare them to topic wordclouds, which are related to the probability of the word existing in an abstract given a topic
## So the relative frequency will do: which is the number of times each unique word occurs in the topic divided by the number of total words in the topic
## Step 4: run function on each topic, get df from each one, and put them together
## Step 5: draw wordclouds
## 

## Step 1 and 2

papers_summ <- cbind.data.frame(doi = as.character(papers_gamma_short$document),
                                topic_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,which.max),
                                gamma_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,max))

papers_summ$top_75 <- NA 
ind_top_75 <- which(papers_summ$gamma_max > 0.75)
papers_summ$top_75[ind_top_75] <- papers_summ$topic_max[ind_top_75]

papers_75 <- papers_summ[!is.na(papers_summ$top_75),]

papers_summ_abstract_75 <- papers_75 %>% left_join(data_decade, by = "doi") %>% select(doi, top_75, abstract)

## Step 3
## 
## get a sample of abstracts: topic 1

word_freq_topic <- function(data, topic_num, path_dict_tools){
  
  # data : whole data.frame
  # topic_num : topic we will focus on
  # path_dict_tools : directory with auxiliary data files
  
  papers_topic <-  data %>% filter(top_75 == topic_num)
  
  # getting a list of clean words used by paper
  df_words <- cleaning_words_abstract(papers_topic, path_dict_tools)
  
  # now compute frequencies!
  word_freq <- df_words %>% dplyr:::count(word_am_lem)
  word_freq <- word_freq %>% mutate(prop = n/nrow(df_words))
  
  word_freq$topic <- rep(topic_num,nrow(word_freq))
  
  return(word_freq)
  
}

# Step 4
freq_word_top_df <- do.call(rbind.data.frame,lapply(1:N_topics,
                                                    function(x) word_freq_topic(papers_summ_abstract_75, 
                                                                                x, path_dict_tools)))
# this is computing relative freq by topic and putting it all in a large data frame

# Step 5

topic_sample <- freq_word_top_df %>% filter(prop > values_thresh) #%>%  select(term

plot_w <- ggplot(topic_sample, aes(label = word_am_lem, size = prop,  
                                  color = prop)) +
  geom_text_wordcloud_area(rm_outside = rm_outside_par, eccentricity = eccentricity_par,grid_margin = 0) + #area_corr_power = 1,
  scale_size_area(max_size = max_size_area) +
  theme_bw() + scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1))+
  facet_wrap(~topic, scales = "free",shrink = F) +
  theme(strip.text = element_text(size = 45),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        strip.background = element_rect(fill = 'white')) 

ggsave(plot=plot_w,filename=filename, height=height_file_par, width = width_file_par, units = "in")
