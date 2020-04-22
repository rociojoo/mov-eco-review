##################################
# Read in libraries and arguments
##################################

# libraries
library(tidyverse)
library(topicmodels)
# paths
path_data <- "./Data/ProcessedQueries/References/"
path.plots <- "./Rocio/Plots/"
path_topics <- "./Data/Topics/"
path_dict_tools <- "./Data/Dictionary/AuxiliaryTextMining/"

### Begin reading in data

# main dataset

papers <- read.csv(file = paste0(path_data,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

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

# Begin analysis

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

papers_summ <- cbind.data.frame(doi = as.character(papers_gamma_short$document),
  topic_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,which.max),
  gamma_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,max))

#
### if we have labels :
if (N_topics == 13){
  if (method_par == "VEM"){
    topic_labels <- c("1_bird","2_forage","3_marine","4_stimulus","5_activity","6_predator","7_model","8_freshwater","9_tests",
      "10_HR","11_HS","12_population","13_motion")
  }
}else if (N_topics == 15){
  if (method_par == "VEM"){
    # topic_labels <- c("1_habitat","2_activity","3_social","4_migration","5_stimulus","6_anthropogenic","7_marine-beh","8_dispersal","9_predator",
    # "10_coll-mov-mod","11_flight","12_fish-mov","13_human-methods","14_player-motion","15_animal-methods")
    # topic_labels <- c("1_predator","2_habitat","3_social","4_activity","5_data","6_model","7_fish","8_sex","9_HR",
    #                   "10_forage","11_human_motion","12_anim_motion","13_tests","14_acoustic_telemetry","15_migration")
    topic_labels <- c("Social interactions and dispersal","Movement models","Habitat selection","Detection and data","Home-ranging",
      "Aquatic systems","Foraging in marine megafauna","Biomechanics","Acoustic telemetry",
      "Experimental designs","Activity budget","Avian migration","Sports","Human activity patterns","Nesting behavior")
  }else{
    topic_labels <- c("1_forage","2_model","3_sound","4_forage_marine","5_data","6_energy","7_freshwater","8_hab_sel","9_HR",
      "10_interaction","11_pop","12_control","13_motion","14_motion_human","15_anthropo")
  }
}

# now we have to count and make a data frame
papers_summ$topic_max <- as.factor(papers_summ$topic_max)
# so I have to merge papers_summ with year info:

papers_summ_year <- papers_summ %>% left_join(data_decade_summ, by = "doi")

topics_count_year <- papers_summ_year %>% 
  group_by(topic_max,pubyear) %>% 
  summarise(n())
colnames(topics_count_year) <- c("Topic","year","num_papers")

# now total of papers in each year (from this dataset)

papers_year <- papers_summ_year %>% group_by(pubyear) %>% summarise(n())
colnames(papers_year) <- c("year","num_papers")

topics_count_year$prop <- topics_count_year$num_papers/rep(papers_year$num_papers,N_topics)
topics_count_year$Topic <- as.factor(topics_count_year$Topic)
levels(topics_count_year$Topic) <- topic_labels 


# We're going to plot our lines and then adjust the color, alpha, and linetype to better read the data

plot_df <- topics_count_year

# Run a quick linear model to measure which trend lines are positive or negative
# we'll reference this when we choose our colors

here <- by(plot_df, plot_df$Topic, function(x)
lm(x$prop ~ x$year)$coefficients[2]
)
plot_df$Topic <- factor(plot_df$Topic, levels= names(sort(here)))
# Create a grouping variable based on this value

grouping <- data.frame(Topic = c(names(here)[here<=0.003 & here>=(-0.003)],names(here)[here<(-0.003)],names(here)[here>0.003]))
grouping$group <- seq_along(grouping$Topic)

plot_df <- merge(plot_df,grouping, by='Topic')

# Now to make our aesthetic features which will be added with scale_*_manual()
# Colors
# Make a color ramp where the amount of 'grays' will determine the highlighted categories
colfunc <- colorRampPalette(c("red",'gray','gray','gray',"blue"))
colorz <- colfunc(nrow(here))
names(colorz) <- names(sort(here))

# line types
# just need to spread linetypes out enough so that the color and alpha can help distinguish as well
# manual
linetypez <- c(1,2,4,3,5,6,1,2,3,6,5,3,4,2,1)
# or random
# linetypez <- rep(1:6,times=ceiling(length(levels(plot_df$Topic))/6))
# linetypez <- linetypez[seq_along(levels(plot_df$Topic))]
names(linetypez) <- names(sort(here))

# alpha
# Changing alpha will help to make the important categories pop.
# Create a gradient of alphas from 1 -> .2 -> so none trend lines are grayed out.
nz <- length(here)
# automatically
alphaz <- c((1*nz/2):(.2*nz/2)/nz*2,(.2*nz/2):(1*nz/2)/nz*2,ifelse(nz%%2==0,NULL,1))
# or manually

alphaz <- c(1,.9,.7,.6,.4,.4,.4,.4,.4,.4,.4,.6,.7,.9,1)
names(alphaz) <- names(sort(here))

# You have to include color, linetype, and alpha in the mapping even if youre going to override it anyway.

ggplot(
data  = plot_df, 
mapping = aes(x = year, y = prop, color = Topic, group = group, linetype = Topic, alpha = Topic)
) +
geom_line(size=1.5) +
scale_color_manual(values = colorz) +
scale_linetype_manual(values = linetypez) +
scale_alpha_manual(values = alphaz)+
theme_bw()+xlab("") + ylab("Proportion of articles in a year") +
theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=15),
axis.title.y = element_text(margin = margin(r=10),size=17), 
axis.title.x = element_text(margin = margin(t=10)),
legend.key.size = unit(2,"line"),
legend.title=element_text(size=16))

#ggsave(paste0(path.plots,"TopicsMax_ts_Ntopics_trendlines_",N_topics,"_method_",method_par,".pdf"), width=16,height=8)


