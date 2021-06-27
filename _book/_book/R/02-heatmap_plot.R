###########################################
# "Making ordered heat map with heatmaply"
##########################################

###  Read in libraries and arguments
# libraries
library(tidyverse)
library(topicmodels)
library(heatmaply)

###  Read in arguments
# Arguments are as follows : 
#   **path_data** : path for cleaned WoS reference csv  
#   **path.plots** : folder path to save plots  
#   **path_topics** : folder of topics rds outputs  

# paths
path_data <- "./Data/Rocio-temporal/cleaned_papers_all_years_simple.csv"
path.plots <- "./Data/Rocio-temporal/"
path_topics <- "./Data/Topics/"

 
# The heat map algorithm is donw pretty straightforward using `heatmaply`. Theres not a ton of options you can edit, but we
# will order the graph by max score to make it look better, as otherwise the algorithm will automatically group for you.

# Now read in data and prepare it

# main dataset

papers <- read.csv(file = path_data,stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

data_decade_summ <- data_decade %>% select(doi,pubyear)

############ ANALYSIS FROM THE TOPICS #################

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

papers_gamma$topic_lab <- plyr::mapvalues(papers_gamma$topic, from = t(as.data.frame(new_order)), to = new_label_for_order)
papers_gamma_onlylab <- select(papers_gamma,-topic) # only reordered labels
papers_gamma_short <- spread(papers_gamma_onlylab, key = topic_lab, value = gamma)
names(papers_gamma_short) <- c("document", paste("Topic", names(papers_gamma_short)[-1])) 


######## HEATMAP ###########
# Create the heatmap.  
# The original heatmap model takes a while to run so be prepared. The flow is to run the model, then you look at the plot with browseURL, then fix and rerun again until its the way you want it. You can also manually save the picture rather than using ggsave.

mat <- papers_gamma_short[,-1]

fname = paste0(path.plots,"heatmaply_plot_",N_topics,".html")

p <- heatmaply(mat[,], file = fname, fontsize_col = 24, plot_method = "ggplot",
  Rowv = TRUE, Colv = FALSE, seriate = 'none')

# browseURL(fname)
#
# Now order it
df1 <- p$x$data[[1]]$z
order(apply(df1,2, function(x) which.max(x)))
rank(apply(df1,2, function(x) which.max(x)))
mat1 <- mat[,order(apply(df1,2, function(x) which.max(x)))]
p <- heatmaply(mat1[,], file = fname, fontsize_col = 24, plot_method = "ggplot",
  Rowv = 1:250, Colv = FALSE)
browseURL(fname)
# Manual fix it (if necessary)
mat2 <- mat1[,]
p <- heatmaply(mat2[,], file = fname, fontsize_col = 20, plot_method = "ggplot",
  Rowv = 1:250, Colv = FALSE,showticklabels=c(T,F), xlab='',ylab='',titleT=FALSE, titleX=TRUE,
  dynamicTicks= FALSE)
# p
# browseURL(fname)
p
# Using ggsave only kind of works, but the ticks and x and y titles suddnely appear
# ggsave(filename=paste0(path.plots,'heatmap3.png'),width = 140*3, height = 78*3, units = 'mm')
# I had the most luck with using export. Though it is finicky