library(topicmodels)
library(tidytext)
library(magrittr)
library(dplyr) # group_by
library(plyr) # mapvalues
library(tidyr) # spread

# paths
path_topics <- "./Data/Topics/"

# main dataset

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

############# WORDCLOUDS ######################

# 1. Topic as a mixture of words
# beta: the probability of that term being generated from that topic.
papers_beta <- tidytext::tidy(modk, matrix = "beta")
# papers_beta
head(papers_beta)

papers_beta$topic_lab <- as.factor(papers_beta$topic)
papers_beta$topic_lab <- mapvalues(papers_beta$topic_lab, from = t(as.data.frame(new_order)), to = new_label_for_order)
papers_beta$topic_lab <- factor(papers_beta$topic_lab, levels = new_label_for_order)


top_words <- papers_beta %>% filter(term != "movement") %>% filter(term != "behavior") %>%   group_by(topic_lab) %>% top_n(n=4,wt=beta)

# creating new sample for word intrusion
new_sample <- sapply(1:N_topics, function(x){
  group <- which(top_words$topic_lab == x)
  words_group <- top_words$term[group]
  new_word <- top_words$term[sample(which(top_words$topic_lab != x),1,replace=FALSE)]
  while (new_word %in% words_group){ # in case the word is the same of any of the 4 above
    new_word <- top_words$term[sample(which(top_words$topic_lab != x),1,replace=FALSE)]
  }
  new_word_group <- c(words_group,new_word)
})

# now sort them alphabetically per group

word_intruded_set <- apply(new_sample,2,sort)

write.csv(word_intruded_set,paste0(path_topics,"word_intrusion/word_intrusion_test.csv"))

# the real group

# top_words$topic_lab <- as.integer(as.character(top_words$topic_lab))

top_ordered <- top_words %>% select(topic_lab, term) %>% arrange(topic_lab,term) 

top_ordered_matrix <- matrix(NA,ncol=N_topics,nrow=4)
for (i in 1:N_topics){
  top_ordered_matrix[,i] <- top_ordered$term[top_ordered$topic_lab == i]
}

write.csv(top_ordered_matrix,paste0(path_topics,"word_intrusion/word_intrusion_real.csv"))

################ ANALYSIS OF THE RESULTS ###################

# loading the real dataset

real_set <- read.csv(paste0(path_topics,"word_intrusion/word_intrusion_real.csv"), header = TRUE)
head(real_set)

test_set <- read.csv(paste0(path_topics,"word_intrusion/word_intrusion_test.csv"), header = TRUE)
head(test_set)
test_set <- test_set[1:5,]

real_intruders <- sapply(2:dim(real_set)[2], function(x){
  setdiff(test_set[,x],real_set[,x])
})


# loading the answers
files_intrusion <- dir(path = paste0(path_topics,"word_intrusion/"), pattern = "word_intrusion_test")

answers <- do.call(rbind.data.frame,lapply(files_intrusion, function(x){
  answer_set <- read.csv(paste0(path_topics,"word_intrusion/",x), header = TRUE)
  comparison <- real_intruders == answer_set[6,2:dim(real_set)[2]]
  comparison[which(is.na(comparison))] <- FALSE # if they didn't answer it counts as wrong (or actually, not right)
  return(comparison)
}))

apply(answers,2,sum)

