##############################
# Making taxa frame bar plots
##############################

library(tidyverse)
library(cowplot)
source("./R/expectation_functions.R")

# Arguments
path <- "./Data/"
path_processed_dictionaries <- "./Data/Dictionary/Papers-Term/"
path_dictionary_info <- "./Data/Dictionary/"

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

framework_labels <- c("External factors", "Internal state", "Motion", "Navigation")

# Now choose dictionary
dictionary <- "Taxonomy"
res <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1, 
                             paper_cat_out=TRUE, filter_lines=TRUE, suffix=NULL, 
                             path_processed_dictionaries)

newtaxa_df <- res[[2]]
rownames(newtaxa_df) <- NULL

# Now choose dictionary
dictionary <- "Framework" # "Humans"
res <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1, 
                             paper_cat_out=TRUE, filter_lines=TRUE, suffix=NULL, 
                             path_processed_dictionaries)

frame_df <- res[[2]]
frame_df$doi <- rownames(frame_df)
rownames(frame_df) <- NULL

frame_taxa <- inner_join(frame_df,newtaxa_df,by="doi")

frame_taxa %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) -> frame_taxa

frame_pivot_taxa <- 
  frame_taxa %>%
  pivot_longer(cols = c("External.factors", "Internal.state.factors",
                        "Motion.process", "Navigation.process"), 
               names_to = "Component_name",
               values_to = "Component_binary") %>% 
  filter(Component_binary != 0) 

frame_taxa_pivot <- 
  frame_pivot_taxa %>% 
  pivot_longer(cols = -c(pubyear, Component_name, Component_binary, doi),
               names_to = "Taxon", values_to = "Presence") %>% 
  filter(Presence == 1)

frame_taxa_pivot_count <- frame_taxa_pivot %>% 
  group_by(Component_name, Taxon) %>% 
  count()


plot_ft <- merge(frame_taxa_pivot_count,data.frame(
  total_frame = tapply(frame_pivot_taxa$Component_binary, 
                       frame_pivot_taxa$Component_name,sum )), 
  by.x='Component_name', by.y = 0)
plot_ft$total_prop <- plot_ft$n/plot_ft$total_frame
plot_ft <- merge(plot_ft,data.frame(Component_name = c(
  "External.factors", "Internal.state.factors",
                        "Motion.process", "Navigation.process"
), framework_labels), by='Component_name')


# Color ramp
color_pallete <- c('#88CCEE', '#44AA99', '#117733', '#332288', '#DDCC77', '#999933','#CC6677', '#882255', '#AA4499', '#DDDDDD')

# Reorder if youd like
plot_ft$Taxon <-  factor(plot_ft$Taxon,
                         levels = names(sort(tapply(plot_ft$n,plot_ft$Taxon, sum),decreasing=T)))

plot_ft$framework_labels <- factor(plot_ft$framework_labels, levels = framework_labels)

# make a dataframe of the n's for each group to add later (the taxon part 
# it's just to make it compatible with ggplot grammar)
ann_text <- plot_ft %>% group_by(framework_labels) %>% 
  slice(1) %>% select(total_frame) %>% 
  mutate(Taxon = 'Humans', total_prop = 0.8)


ggplot(data=plot_ft, aes(x=Taxon, y=total_prop, fill=Taxon)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = color_pallete) +
  facet_wrap(facets = vars(framework_labels)) +
  geom_text(data = ann_text,label = paste0('(n = ',ann_text$total_frame,')'), size = 5) +
  theme_bw() + 
  theme(axis.title.y = element_text(size=20),
        axis.text.y = element_text(size=16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 16),
        legend.title=element_text(size=16),
        legend.text = element_text(size = 16)) + 
  ylab('Total proportion of papers tackling each MEF component') 


