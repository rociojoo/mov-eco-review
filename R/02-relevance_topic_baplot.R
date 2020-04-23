# libraries
# library(tidyverse)
library(plyr)
library(dplyr)
library(tidyr) # spread
library(ggplot2)
library(readr)
# library(tm)
library(tidytext)
library(topicmodels)
library(ggwordcloud)
library(cowplot)
library(viridis)
library(RColorBrewer)
library(stringr) # to use str_detect for numbers
library(textstem) # to use lemmatize_words
library(heatmaply)
# library(gridExtra)
# library(grid)

# paths
path_data <- "./Data/ProcessedQueries/References/"
path.plots <- "./Rocio/Plots/"
path_topics <- "./Data/Topics/"
path_dict_tools <- "./Data/Dictionary/AuxiliaryTextMining/"

# calling auxiliary functions
source("./R/Americanizing.R")
source("./R/cleaning_words_abstract.R")

# main dataset

papers <- read.csv(file = paste0(path_data,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

data_decade_summ <- data_decade %>% select(doi,pubyear)

############ ANALYSIS FROM THE TOPICS #################

# Wordcloud of each topic - done

# Number of papers with each topic as its main topic + ts

# Number of papers with related to at least 0.25 to the topic + ts

# Number of papers with related to at least 0.33 to the topic + ts

# Number of papers with related to at least 0.5 to the topic + ts

# put these as columns in the large super matrix with Dimensions info + ts

# Stuff to discuss: 1) threshold on the frequency of words; 2) VEM vs. Gibbs; 3) a penalizing criterion; 
# 4) why in the example 2 is a good number? RUN FOR SEVERAL NUMBER OF TOPICS AGAIN WITH VEM; 5)what to show in Rladies; 


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

# values_thresh <- 0.005
color_pal <- "BYR"

height_file_par <- 26
width_file_par <- 26


if (is.null(alpha_par)){
  filename <- paste0("wordcloud_topics_",N_topics,".pdf")
  # dev.off()
}else{
  filename <- paste0("wordcloud_topics_",N_topics,"_alpha_",alpha_par,"_method_",method_par,"_col_",color_pal,"_B_order_II.pdf")
}


word_clouds_ggsave <- function(data, values_thresh, indiv_angle = FALSE, color_pal = "D", max_size_area = 30, 
                               height_file_par = 26, width_file_par = 26, path_plot, filename, 
                               rm_outside_par = FALSE, eccentricity_par = 1){
  
  # data : df with words (as term column) and frequencies (as beta column) by topic (as topic column)
  # values_tresh : lower limit for frequencies of words
  # indiv_angle : TRUE if each word would have a different angle which could be (-90, -45, 0, 45, 90). 
  #               FALSE if every word is horizontal
  # color_pal : from viridis, check viridis help
  # max_size_area : to plot each wordcloud. Depends on number of words. If too large, there could be a lot of
  #                 space between wordclouds. If too small, some words may not appear in the wordcloud
  # height_file_par and width_file_par : height and width of file where we save the plots (in inches)
  # filename : for the pdf with wordclouds
  # eccentricity_par : proportion of horizontal display of wordcloud (respect to vertical). 
  #                     Default 1 (sphere or square like shape)
  # rm_outside_par : removes text that would not be fitted. TRUE to avoid overlap. But try not to loose text. Default FALSE.
  
  topic_sample <- data %>% filter(beta > values_thresh) #%>%  select(term,beta)
  
  if (indiv_angle == TRUE){
    table_topic <- topic_sample %>%
      mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 2, 4, 2, 1)))
  }else{
    table_topic <- topic_sample %>%
      mutate(angle = rep(0,nrow(topic_sample)))
  }
  # library(RColorBrewer)
  color_pallete <- c('deepskyblue','dodgerblue4')
  color_pallete <- c(low='#ffffcc',high='#253494')
  # #plot_w <- 
  #   ggplot(subset(table_topic,topic==1), aes(label = term, size = beta, angle = angle, 
  #                                   color = beta)) +
  #   geom_text_wordcloud_area(rm_outside = rm_outside_par, eccentricity = eccentricity_par) + #area_corr_power = 1,
  #   scale_size_area(max_size = 30) +
  #   theme_bw() +scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1)) +
  #     ggtitle('ddd')
  #     #scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1))
  #     #scale_colour_gradientn(colors=c('#253494','#edf8b1','#dd1c77'))
  #   
  
  plot_w <- ggplot(table_topic, aes(label = term, size = beta, angle = angle, 
                                    color = beta)) +
    geom_text_wordcloud_area(rm_outside = rm_outside_par, eccentricity = eccentricity_par,grid_margin = 0) + #area_corr_power = 1,
    scale_size_area(max_size = max_size_area) +
    theme_bw() + scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1))+
    facet_wrap(~topic_lab, scales = "free",shrink = F) +
    theme(strip.text = element_text(size = 45),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          strip.background = element_rect(fill = 'white')) 
  # scale_color_viridis(direction = -1, option = 'cividis', end = 0.5)
  # colorRampPalette(c('deepskyblue', 'dodgerblue4'))(sum(table_topic$topic==1)) 
  
  ggsave(plot=plot_w,filename=paste0(path_plot, filename), height=height_file_par, width = width_file_par, units = "in")
  
  ##############
  #   # Do it with cowplot
  # This is if you want scaled plots, but it wont work very well in a function set up
  #   plot_list <- lapply(seq_len(length(unique(table_topic$topic))), function(y) {
  #     x <- unique(table_topic$topic)[y]
  #     size <- c(20,33,28,23,22,18,28,21,28,26,21,20,20,24,16)
  #     ggplot(subset(table_topic, topic==x), aes(label = term, size = beta, angle = angle, 
  #     color = beta)) +
  #     geom_text_wordcloud_area(rm_outside = rm_outside_par, eccentricity = eccentricity_par) + #area_corr_power = 1,
  #    scale_size_area(max_size = size[y]) +
  #     theme_bw() +scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1),
  #       limits = c(min(table_topic$beta),max(table_topic$beta)))+
  #     theme(
  #       plot.margin = margin(t = 0, r = 0, b = 20, l = 0, unit = "pt"),
  #       plot.title = element_text(size=44, hjust = 0.5, vjust=0.5)) + 
  #       ggtitle(x)
  #     }) 
  #   plot_list <- plot_list[order(sapply(plot_list, function(x) x$data$topic[1]))]
  # 
  # plot_w1 <- plot_grid(plotlist=plot_list, ncol = 4)
  # 
  # ggsave(plot=plot_w1,filename=paste0(path_plot, filename2), height=height_file_par, width = width_file_par, units = "in")
  
  
  #   
  # if (is.null(alpha_par)){
  #   ggsave(plot=plot_w,filename=paste0(path_plot, filename), height=height_file_par, width = width_file_par, units = "in")
  #   # dev.off()
  # }else{
  #   ggsave(plot=plot_w,filename=paste0(path_plot, filename), height=height_file_par, width = width_file_par, units = "in")
  #   
  # }
  # 
  
}



word_clouds_ggsave(data = papers_beta, values_thresh = 0.003, indiv_angle = FALSE, color_pal = color_pal, max_size_area = 25, 
                   height_file_par = 26, width_file_par = 26, path_plot = path.plots, filename = filename,
                   rm_outside_par = FALSE, eccentricity_par = 1)


########## GRAPHS WITH GAMMAS ##################


# Long format to short format
papers_gamma_onlylab <- select(papers_gamma,-topic) # only reordered labels
papers_gamma_short <- spread(papers_gamma_onlylab, key = topic_lab, value = gamma)
names(papers_gamma_short) <- c("document", paste("Topic", names(papers_gamma_short)[-1])) 

# ######## HUNTING BEARS ########
# papers_summ <- cbind.data.frame(doi = as.character(papers_gamma_short$document),
#                                 topic_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,which.max),
#                                 gamma_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,max))
# doi_bears <- papers_summ$doi[which(papers_summ$topic_max == 13 & papers_summ$gamma_max > 0.5)]
# ind_papers_bears <- which(data_decade$doi %in% as.character(doi_bears) == TRUE)
# # set_abstracts_complete$doi[seq(from=1+N_samples*(topic-1),to=N_samples*topic,by=1)] <- data_decade$doi[ ind_papers ]
# # set_abstracts_complete$abstract[seq(from=1+N_samples*(topic-1),to=N_samples*topic,by=1)] <- data_decade$abstract[ ind_papers ]
# write.table(data_decade$abstract[ind_papers_bears], file = paste0(path.plots,"abstracts_13_75.txt"))

# ######## HEATMAP HERE! ###########
# mat <- papers_gamma_short[,-1]
# 
# fname = paste0(path.plots,"heatmaply_plot_",N_topics,".html")
# # fname = paste0(path.plots,"heatmaply_plot_",N_topics,".pdf")
# 
# p <- heatmaply(mat[,], file = fname, fontsize_col = 24, plot_method = "ggplot",
#   Rowv = TRUE, Colv = FALSE, seriate = 'none')
# # 
# # browseURL(fname)
# # 
# # Now order it
# df1 <- p$x$data[[1]]$z
# order(apply(df1,2, function(x) which.max(x)))
# rank(apply(df1,2, function(x) which.max(x)))
# mat1 <- mat[,order(apply(df1,2, function(x) which.max(x)))]
# p <- heatmaply(mat1[,], file = fname, fontsize_col = 24, plot_method = "ggplot",
#   Rowv = 1:250, Colv = FALSE)
# # browseURL(fname)
# # Manual fix it (if necessary)
# mat2 <- mat1[,]
# p <- heatmaply(mat2[,], file = fname, fontsize_col = 24, plot_method = "ggplot",
#   Rowv = 1:250, Colv = FALSE,showticklabels=c(T,F))
# # p
# browseURL(fname)
# # Using ggsave only kind of works, but the ticks and x and y titles suddnely appear
ggsave(paste0(path.plots,'heatmap3.png'),width = 140*3, height = 78*3, units = 'mm')
# ####################################

# apply(papers_gamma_short[,2:14],2,sum)

papers_summ <- cbind.data.frame(doi = as.character(papers_gamma_short$document),
                                topic_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,which.max),
                                gamma_max = apply(papers_gamma_short[,2:ncol(papers_gamma_short)],1,max))



# now we have to count and make a data frame
papers_summ$topic_max <- as.factor(papers_summ$topic_max)
topics_count <- papers_summ %>% select(doi, topic_max) %>% 
  group_by(topic_max) %>% 
  dplyr::summarise(n())
colnames(topics_count) <- c("Topic","num_papers")

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
topics_count$Topic <- topic_labels

topics_count <- topics_count %>% 
  # arrange(desc(num_papers))
  arrange(num_papers)
# taxo_plot <- taxo_plot[1:6,]
# taxo_plot <- taxo_plot[(nrow(taxo_plot)-5):nrow(taxo_plot),]
topics_count$Topic <- factor(topics_count$Topic,levels=topics_count$Topic)



ggplot(topics_count, aes(x=Topic,y=num_papers)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  xlab("Topic") + ylab("Number of articles") +
  theme_cowplot() + theme(text = element_text(size=20),axis.text.y = element_text(size=16))
ggsave(filename=paste0(path.plots,"TopicMax_barplot_flip_Ntopics_",N_topics,"_method_",method_par,".pdf"), height=7,width=7)

## now, TS:

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

values_prop <- sort(unique(topics_count_year$prop))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.02)
values_year <- seq(from=min(topics_count_year$year), to=max(topics_count_year$year),by=1)
# topics_count_year$line_t <- as.character(rep(c("solid","dashed","dotted"),each=nrow(topics_count_year)/3))

# line_lab <- topics_count_year$line_t[which(duplicated(topics_count_year$Topic) == FALSE)]

# color.pallete <- rep(brewer.pal(5, "Paired"),3) # color palette
ggplot(data  = topics_count_year, mapping = aes(x = year, y = prop, color = Topic, linetype = Topic)) +
  geom_line(size=1.5) +
  # scale_colour_manual(name="category", values = color.pallete) + #breaks = corrected_df$col,
  # labels=corrected_df$category) +
  scale_x_continuous(breaks = values_year) +
  scale_y_continuous(breaks = values_breaks) +
  scale_linetype_manual(name = "Topic", values = rep(c(1:3),5)) +
  xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=16),
        axis.title.y = element_text(margin = margin(r=10)), 
        axis.title.x = element_text(margin = margin(t=10)))
ggsave(paste0(path.plots,"TopicsMax_ts_Ntopics_",N_topics,"_method_",method_par,".pdf"), width=16,height=8)


############### NOW ONLY FOR PAPERS RELATED TO TOPICS BY MORE THAN 0.5 #############

papers_summ$top_50 <- NA 
ind_top_50 <- which(papers_summ$gamma_max > 0.5)
papers_summ$top_50[ind_top_50] <- papers_summ$topic_max[ind_top_50]

papers_50 <- papers_summ[!is.na(papers_summ$top_50),]

# now we have to count and make a data frame

topics_count_50 <- papers_50 %>% select(doi, top_50) %>% 
  group_by(top_50) %>% 
  summarise(n())
colnames(topics_count_50) <- c("Topic","num_papers")

# ### if we have labels :
# 
# topic_labels <- c("1_data","2_sound","3_method","4_dispersal","5_nest","6_freshwater","7_marine","8_energy","9_habitat",
#                   "10_interaction","11_bird","12_sex","13_motion","14_forage","15_home_range")
topics_count_50$Topic <- topic_labels

topics_count_50 <- topics_count_50 %>% 
  # arrange(desc(num_papers))
  arrange(num_papers)
# taxo_plot <- taxo_plot[1:6,]
# taxo_plot <- taxo_plot[(nrow(taxo_plot)-5):nrow(taxo_plot),]
topics_count_50$Topic <- factor(topics_count_50$Topic,levels=topics_count_50$Topic)

# topics_count_50$num_papers/dim(papers_gamma_short)[1]*100

ggplot(topics_count_50, aes(x=Topic,y=num_papers)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  xlab("Topic") + ylab("Number of articles") +
  theme_cowplot() + theme(text = element_text(size=20),axis.text.y = element_text(size=16))
ggsave(filename=paste0(path.plots,"Topic50_barplot_flip_method_",method_par,".pdf"), height=7,width=7)

## now, TS:

# so I have to merge papers_summ with year info:

papers_summ_year_50 <- papers_50 %>% left_join(data_decade_summ, by = "doi")

topics_count_year_50 <- papers_summ_year_50 %>% 
  group_by(topic_max,pubyear) %>% 
  summarise(n())
colnames(topics_count_year_50) <- c("Topic","year","num_papers")


# now total of papers in each year (from this dataset)

papers_year_50 <- papers_summ_year_50 %>% group_by(pubyear) %>% summarise(n())
colnames(papers_year_50) <- c("year","num_papers")

topics_count_year_50$prop <- topics_count_year_50$num_papers/rep(papers_year_50$num_papers,N_topics)
topics_count_year_50$Topic <- as.factor(topics_count_year_50$Topic)
levels(topics_count_year_50$Topic) <- topic_labels 

values_prop <- sort(unique(topics_count_year_50$prop))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.02)
values_year <- seq(from=min(topics_count_year_50$year), to=max(topics_count_year_50$year),by=1)
# topics_count_year$line_t <- as.character(rep(c("solid","dashed","dotted"),each=nrow(topics_count_year)/3))

# line_lab <- topics_count_year$line_t[which(duplicated(topics_count_year$Topic) == FALSE)]

# color.pallete <- rep(brewer.pal(5, "Paired"),3) # color palette
ggplot(data  = topics_count_year_50, mapping = aes(x = year, y = prop, color = Topic, linetype = Topic)) +
  geom_line(size=1.5) +
  # scale_colour_manual(name="category", values = color.pallete) + #breaks = corrected_df$col,
  # labels=corrected_df$category) +
  scale_x_continuous(breaks = values_year) +
  scale_y_continuous(breaks = values_breaks) +
  scale_linetype_manual(name = "Topic", values = rep(c(1:3),5)) +
  xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=16),
        axis.title.y = element_text(margin = margin(r=10)), 
        axis.title.x = element_text(margin = margin(t=10)))
ggsave(paste0(path.plots,"Topics50_ts.pdf"), width=12,height=8)


############### NOW ONLY FOR PAPERS RELATED TO TOPICS BY MORE THAN 0.75 #############

papers_summ$top_75 <- NA 
ind_top_75 <- which(papers_summ$gamma_max > 0.75)
papers_summ$top_75[ind_top_75] <- papers_summ$topic_max[ind_top_75]

papers_75 <- papers_summ[!is.na(papers_summ$top_75),]

# now we have to count and make a data frame

topics_count_75 <- papers_75 %>% select(doi, top_75) %>% 
  group_by(top_75) %>% 
  summarise(n())
colnames(topics_count_75) <- c("Topic","num_papers")

# ### if we have labels :
# 
# topic_labels <- c("1_data","2_sound","3_method","4_dispersal","5_nest","6_freshwater","7_marine","8_energy","9_habitat",
                  # "10_interaction","11_bird","12_sex","13_motion","14_forage","15_home_range")
# topic_labels <- c("1_Predator-prey","2_Habitat selection","3_Social interactions",
                  # "4_Physical activity","5_Detection and data","6_freshwater","7_marine","8_energy","9_habitat",
# "10_interaction","11_bird","12_sex","13_motion","14_forage","15_home_range")
topics_count_75$Topic <- topic_labels

topics_count_75 <- topics_count_75 %>% 
  # arrange(desc(num_papers))
  arrange(num_papers)
# taxo_plot <- taxo_plot[1:6,]
# taxo_plot <- taxo_plot[(nrow(taxo_plot)-5):nrow(taxo_plot),]
topics_count_75$Topic <- factor(topics_count_75$Topic,levels=topics_count_75$Topic)

# topics_count_75$num_papers/dim(papers_gamma_short)[1]*100

ggplot(topics_count_75, aes(x=Topic,y=num_papers)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  xlab("Topic") + ylab("Number of articles") +
  theme_cowplot() + theme(text = element_text(size=20),axis.text.y = element_text(size=16))
ggsave(filename=paste0(path.plots,"Topic75_barplot_flip.pdf"), height=7,width=7)

## now, TS:

# so I have to merge papers_summ with year info:

papers_summ_year_75 <- papers_75 %>% left_join(data_decade_summ, by = "doi")

topics_count_year_75 <- papers_summ_year_75 %>% 
  group_by(topic_max,pubyear) %>% 
  summarise(n())
colnames(topics_count_year_75) <- c("Topic","year","num_papers")

# # Apparently a year is missing... 
# 
missing_lab <- setdiff(paste0(rep(1:N_topics,each=10),"-",rep(2009:2018,N_topics)),paste0(topics_count_year_75$Topic,"-",topics_count_year_75$year))

topic_miss <- as.numeric(substr(missing_lab,1,nchar(missing_lab)-5))
year_miss <- as.numeric(substr(missing_lab,nchar(missing_lab)-3,nchar(missing_lab)))

topics_count_year_75_comp <- rbind.data.frame(cbind.data.frame(Topic=topic_miss,year=year_miss,num_papers=rep(0,length(missing_lab))),topics_count_year_75) %>% arrange(Topic,year)

topics_count_year_75_comp$Topic <- as.numeric(topics_count_year_75_comp$Topic)
topics_count_year_75_comp <- topics_count_year_75_comp %>%
  # arrange(desc(num_papers))
  arrange(Topic,year)

papers_year$year <- factor(papers_year$year,levels = sort(papers_year$year))
# 

# now total of papers in each year (from this dataset)

# topics_count_year_75_comp <- topics_count_year_75 # if nothing is missing now

papers_year_75 <- papers_summ_year_75 %>% group_by(pubyear) %>% summarise(n())
colnames(papers_year_75) <- c("year","num_papers")

topics_count_year_75_comp$prop <- topics_count_year_75_comp$num_papers/rep(papers_year_75$num_papers,N_topics)
topics_count_year_75_comp$Topic <- as.factor(topics_count_year_75_comp$Topic)
levels(topics_count_year_75_comp$Topic) <- topic_labels 

values_prop <- sort(unique(topics_count_year_75_comp$prop))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.05)
values_year <- seq(from=min(topics_count_year_75_comp$year), to=max(topics_count_year_75_comp$year),by=1)
# topics_count_year$line_t <- as.character(rep(c("solid","dashed","dotted"),each=nrow(topics_count_year)/3))

# line_lab <- topics_count_year$line_t[which(duplicated(topics_count_year$Topic) == FALSE)]

# color.pallete <- rep(brewer.pal(5, "Paired"),3) # color palette
ggplot(data  = topics_count_year_75_comp, mapping = aes(x = year, y = prop, color = Topic, linetype = Topic)) +
  geom_line(size=1.5) +
  # scale_colour_manual(name="category", values = color.pallete) + #breaks = corrected_df$col,
  # labels=corrected_df$category) +
  scale_x_continuous(breaks = values_year) +
  scale_y_continuous(breaks = values_breaks) +
  scale_linetype_manual(name = "Topic", values = rep(c(1:3),5)) +
  xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=16),
        axis.title.y = element_text(margin = margin(r=10)), 
        axis.title.x = element_text(margin = margin(t=10)))
ggsave(paste0(path.plots,"Topics75_ts.pdf"), width=12,height=8)


##################################

saveRDS(papers_summ,paste0(path_topics,"Topics_Paper.rds"))

# per topic, get one representative paper
# papers_summ$topic_max <- as.factor(papers_summ$topic_max)
# levels(papers_summ$topic_max)

# # Wrong attempt
# best_sample <- papers_summ %>% group_by(topic_max) %>% summarise(doi[which.max(gamma_max)])
# colnames(best_sample) <- c("Topic","doi")

# Correction
best_sample <-papers_summ %>% 
  group_by(topic_max) %>% 
  slice(which.max(gamma_max))
names(best_sample)
#colnames(best_sample) <- c("Topic","doi")

#  summarise(which.max(gamma_max))
  
#which.max(papers_summ$gamma_max[papers_summ$topic_max == 1])  

i = 13 # this has nothing to do with the real topic!!!! something about factors? or the model is doing things wrong?
papers_gamma_short[papers_summ$doi == best_sample$doi[i],]
data_decade$abstract[which(data_decade$doi == best_sample$doi[i])]
# It makes sense with Gibbs, how come it doesn't with VEM?

############################ create a sample of papers associated to each topic #########################

N_samples <- 5

set_abstracts <- data.frame(topic = rep(1:N_topics,each=N_samples), doi = rep(NA,N_topics*N_samples), 
                            abstract = rep(NA,N_topics*N_samples)) #, label = rep(topic_labels,each=N_samples))

# new_cols <- colnames(papers_gamma_short)[2:(N_topics + 1)]

set_abstracts_complete <- cbind.data.frame(set_abstracts, matrix(NA,ncol=N_topics,nrow=N_topics*N_samples))

for (topic in 1:N_topics){
  # ind_topic <- which(papers_summ$top_75 == topic)
  ind_topic <- which(papers_summ$topic_max == topic)
  # muestra <- sample(ind_topic, 5, replace = FALSE)
  muestra <- ind_topic[order(papers_summ$gamma_max[ind_topic],decreasing = TRUE)][1:N_samples] #ind_topic[which(order(papers_summ$gamma_max[ind_topic],decreasing = TRUE) %in% 1:N_samplesind]
  ind_papers <- which(data_decade$doi %in% as.character(papers_summ$doi[muestra]) == TRUE)
  set_abstracts_complete$doi[seq(from=1+N_samples*(topic-1),to=N_samples*topic,by=1)] <- data_decade$doi[ ind_papers ]
  set_abstracts_complete$abstract[seq(from=1+N_samples*(topic-1),to=N_samples*topic,by=1)] <- data_decade$abstract[ ind_papers ]
  ind_gamma <- which(papers_gamma_short$document %in% papers_summ$doi[muestra] == TRUE)
  set_abstracts_complete[seq(from=1+N_samples*(topic-1),to=N_samples*topic,by=1), 4:ncol(set_abstracts_complete)] <- 
    papers_gamma_short[ind_gamma, 2:ncol(papers_gamma_short)]
    
}

write_csv(set_abstracts_complete,paste0(path_topics,"sample_validation_top_Ntopics_",N_topics,"_method_",method_par,"_II.csv"))

############################## I want now to make WORD CLOUDS OF ABSTRACTS OF EACH TOPIC ##############################
## Step 1: select papers that have > 0.75 gamma for a topic
## Step 2: join with abstracts
## Step 3: write a function to: for each set of abstracts (topic), 
## process words and compute frequencies
## the thing is that, word frequency in a topic is related to the number of abstracts related to the topic
## and we will compare them to topic wordclouds, which are related to the probability of the word existing in an abstract
## given a topic
## So the relative frequency will do: which is the number of times each unique word occurs in the topic 
## divided by the number of total words in the topic
## Step 4: run function on each topic, get df from each one, and put them together
## Step 5: draw wordclouds
## 

## Step 1 and 2

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

# To use this function, I need to name the frequency column as beta, 
# the word_am_lem column as term
# and topic as topic (that one's fine)

names(freq_word_top_df) <- c("term","n","beta","topic_lab")

word_clouds_ggsave(data = freq_word_top_df, values_thresh = 0.003, indiv_angle = FALSE, color_pal = "D", max_size_area = 30, 
                   height_file_par = 30, width_file_par = 30, path_plot = path.plots, 
                   filename = paste0("wordcloud_topics_", N_topics, "_abstracts_II.pdf"), 
                   rm_outside_par = FALSE, eccentricity_par = 1)

################ FINDING OUT HOW MANY PAPERS ARE NOT "REALLY" ANYWHERE ##############
## Idea: we need a criterion to say if most papers are related to at least one topic or just "randomly" related ##
## We first compute the median of gammas for each topic, 
## then see how many papers are below the median for every single topic
## 

(topic_median <- apply(papers_gamma_short[,-1],2,median))
hist(pull(papers_gamma_short,6),breaks=100)
class(as.numeric(papers_gamma_short[,6]))
hist(papers_gamma_short[,6],breaks = 100)

topic_median <- rep(0.3,13)
# now make df with it: 
papers_low <- papers_gamma_short[,-1] < matrix(rep(topic_median, nrow(papers_gamma_short)), nrow = nrow(papers_gamma_short), byrow = TRUE)
sum(unlist(apply(papers_low, 1, function(x) if (all(x) == TRUE){return(1)})))

################ CALCULATE AN IMPORTANCE MEASURE FOR THE TOPICS (SUM OF GAMMAS) ###################
# Idea: I take the gamma matrix and then sum up for each column, then table and barplot
# 
gamma_sum <- apply(papers_gamma_short[,-1],2,sum)
# gamma_sum_df <- data.frame(Topic = topic_labels, Gamma = gamma_sum)
gamma_sum_df <- data.frame(Topic = 1:15, Gamma = gamma_sum)
gamma_sum_ordered <- gamma_sum_df[order(gamma_sum_df$Gamma, decreasing = FALSE), ]
gamma_sum_ordered$Topic <- factor(gamma_sum_ordered$Topic, levels = gamma_sum_ordered$Topic)


ggplot(gamma_sum_ordered, aes(x=Topic,y=Gamma)) +
  geom_bar(stat="identity",position="identity") +
  coord_flip() +
  xlab("Topic") + ylab("Total gamma") +
  theme_cowplot() + theme(text = element_text(size=20),axis.text.y = element_text(size=16))
ggsave(filename=paste0(path.plots,"TopicGamma_barplot_flip_Ntopics_",N_topics,"_method_",method_par,"_nolabels.pdf"), height=7,width=7)

## series of gammas
## 

papers_gamma_doi <- papers_gamma
names(papers_gamma_doi) <- c("doi",names(papers_gamma)[2:length(names(papers_gamma))])
papers_gamma_sum_year <- papers_gamma_doi %>% left_join(data_decade_summ, "doi")

gamma_sum_year <- papers_gamma_sum_year %>% 
  group_by(topic_lab,pubyear) %>% 
  summarise(sum(gamma))
colnames(gamma_sum_year) <- c("Topic","year","gamma")
gamma_sum_year$Topic <- mapvalues(gamma_sum_year$Topic, from = t(as.data.frame(1:N_topics)), to = topic_labels)
gamma_sum_year$prop <- gamma_sum_year$gamma/rep(papers_year$num_papers,N_topics)

levels(gamma_sum_year$Topic) <- topic_labels 

values_prop <- sort(gamma_sum_year$prop)
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.05)
values_year <- seq(from=min(gamma_sum_year$year), to=max(gamma_sum_year$year),by=1)
# topics_count_year$line_t <- as.character(rep(c("solid","dashed","dotted"),each=nrow(topics_count_year)/3))

# line_lab <- topics_count_year$line_t[which(duplicated(topics_count_year$Topic) == FALSE)]

# color.pallete <- rep(brewer.pal(5, "Paired"),3) # color palette
ggplot(data  = gamma_sum_year, mapping = aes(x = year, y = prop, color = Topic, linetype = Topic)) +
  geom_line(size=1.5) +
  # scale_colour_manual(name="category", values = color.pallete) + #breaks = corrected_df$col,
  # labels=corrected_df$category) +
  scale_x_continuous(breaks = values_year) +
  scale_y_continuous(breaks = values_breaks) +
  scale_linetype_manual(name = "Topic", values = rep(c(1:3),5)) +
  xlab("") + ylab("Sum of gamma for each topic divided by the number of papers in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=16),
        axis.title.y = element_text(margin = margin(r=10)), 
        axis.title.x = element_text(margin = margin(t=10)))
ggsave(paste0(path.plots,"TopicsGamma_ts.pdf"), width=16,height=8)


