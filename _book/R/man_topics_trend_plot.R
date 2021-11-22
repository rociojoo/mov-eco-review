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

modk <- readRDS(file = paste0(path_topics,"NewBestTopicModel",N_topics,"_alpha_",alpha_par,"_method_",method_par,"_filtered_II.rds"))

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
topic_labels <- c("Social interactions and dispersal","Movement models","Habitat selection","Detection and data","Home ranges",
                  "Aquatic systems","Foraging in marine megafauna","Biomechanics","Acoustic telemetry",
                  "Experimental designs","Activity budgets","Avian migration","Sports","Human activity patterns","Breeding ecology")


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

papers_gamma_doi <- papers_gamma
names(papers_gamma_doi) <- c("doi",names(papers_gamma)[2:length(names(papers_gamma))])
papers_gamma_sum_year <- papers_gamma_doi %>% left_join(data_decade_summ, "doi")

gamma_sum_year <- papers_gamma_sum_year %>% 
  group_by(topic_lab,pubyear) %>% 
  summarise(sum(gamma))
colnames(gamma_sum_year) <- c("Topic","year","gamma")
gamma_sum_year$Topic <- plyr::mapvalues(gamma_sum_year$Topic, from = t(as.data.frame(1:N_topics)), to = topic_labels)
gamma_sum_year$prop <- gamma_sum_year$gamma/rep(papers_year$num_papers,N_topics)

levels(gamma_sum_year$Topic) <- topic_labels 

#####################
# Plotting
# We're going to plot our lines and then adjust the color, alpha, and linetype to better read the data

plot_df <- gamma_sum_year

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
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')
# Okabe_Ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
# colfunc <- colorRampPalette(c("red",'gray','gray','gray',"blue"))
# colorz <- colfunc(nrow(here))
colorz <- Tol_muted[1:(length(here)+1) %% (length(Tol_muted)+1)]
# change problematic colors to gray60
colorz[5] <- "#7f7f7f"
colorz[10] <- "#7f7f7f"
colorz[11] <- "#000000"
#colorz <- Okabe_Ito[1:(length(here)+1) %% (length(Okabe_Ito)+1)]
names(colorz) <- names(sort(here))
colorz

# line types
# just need to spread linetypes out enough so that the color and alpha can help distinguish as well
# manual 5 = dash, 3 = dotted, 1 = solid
linetypez <- c(5,5,3,3,3,3,3,3,3,3,3,3,3,1,1)
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

alphaz <- c(1,.7,.45,.45,.45,.45,.45,.45,.45,.45,.45,.45,.45,.7,1)
names(alphaz) <- names(sort(here))


# line width
sizez <-  c(1,1,2,1,1,1,1,1,1,2,1,2,1,1,2)
names(sizez) <- names(here)
sizez
sizez <- rep(sizez, each = 10)
sizez
# You have to include color, linetype, and alpha in the mapping even if youre going to override it anyway.

p <- ggplot(
  data  = plot_df
) +
  geom_line(size=sizez, 
            mapping = aes(x = year, y = prop, color = Topic, group = group, linetype = Topic, alpha = Topic)) +
  scale_color_manual(values = colorz) +
  scale_linetype_manual(values = linetypez) +
  scale_alpha_manual(values = alphaz)+
  theme_classic()+xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1,size=16),axis.text.y = element_text(size=16),
        legend.position = "none", legend.justification = "right",legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(r=10),size=17), 
        axis.title.x = element_text(margin = margin(t=10)),
        legend.key.size = unit(2,"line"),
        legend.title=element_text(size=16))

start_pos <- plot_df %>% group_by(Topic) %>% summarise(y = last(prop)) %>% mutate(x = 2018)
start_pos$colorz <- colorz
start_pos

#start_pos$x_new <- start_pos$x + c(0.49,0.55,1.0,0.5,0.61,0.57,0.48,0.57,0.97,0.75,0.53,0.61,0.69,0.62,0.26)
start_pos$x_new <- start_pos$x + 0.1
start_pos$y_new <- start_pos$y + c(0.004,-0.003,0,0.004,0.0024,0.001,0.0022,0,0.0043,0,0.000,0,0.0022,0,0.002)
p + geom_text(data = start_pos, aes(x =x_new ,y=y_new, label = Topic), color=colorz,hjust=0,size=5.5)+
  coord_cartesian(xlim = c(2009, 2018),clip = 'off') + 
  theme(plot.margin = unit(c(1,13,1,1), "lines"))

ggsave(filename=paste0(path.plots,"TopicsGamma_ts1.png"), height=10,width=18)
# ggsave("Manuscript/Images/TopicsGamma_ts1.png", width=16,height=10)