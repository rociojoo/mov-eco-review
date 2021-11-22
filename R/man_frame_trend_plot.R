#############################
# Making category line plots
#############################

library(tidyverse)
library(cowplot)
library(paletteer)
source("./R/expectation_functions.R")

# Arguments
path <- "./Data/"
path.plots <- "./Rocio/Plots/"
path_processed_dictionaries <- "./Data/Dictionary/Papers-Term/"

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)

######## Category #############

dictionary <- "Framework"
corrected_df_all <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1,
  paper_cat_out=FALSE, filter_lines=FALSE, suffix=NULL,
  path_processed_dictionaries)

corrected_df_all <- corrected_df_all %>% 
  mutate(category = as.factor(case_when(
    category == "External.factors" ~ "External factors",
    category == "Internal.state.factors" ~ "Internal state",
    category == "Motion.process" ~ "Motion",
    category == "Navigation.process" ~ 'Navigation'
  )))

values_prop <- sort(unique(corrected_df_all$prop_papers))
values_breaks <- seq(from=0,to=max(values_prop)+0.1,by=0.1)
values_year <- seq(from=min(corrected_df_all$year), to=max(corrected_df_all$year),by=1)

comp_colors <- paletteer_d("colorblindr::OkabeIto") %>%
  as.vector()
comp_labels <- levels(corrected_df_all$category) 
names(comp_colors) <- comp_labels
comp_color_palette_current <- comp_colors[unique(corrected_df_all$category)]

##############################
# Plotting
##############################

ggplot(data  = corrected_df_all, mapping = aes(x = year, y = prop_papers, color = category, fill = category)) +
  geom_line(size=1.5, linetype = 3) +
  geom_point(size = 5.5) +
  scale_color_manual(name="Component",values = comp_color_palette_current) +
  scale_fill_manual(name="Component",values = comp_color_palette_current) +
  scale_x_continuous(breaks = values_year) +
  scale_y_continuous(breaks = values_breaks) +
  theme_bw()+xlab("") + ylab("Proportion of articles in a year") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5,size=16),axis.text.y = element_text(size=16),
        legend.position = "bottom", legend.justification = "right",legend.text=element_text(size=15),
        axis.title.y = element_text(margin = margin(r=10),size=17), 
        axis.title.x = element_text(margin = margin(t=10)),
        legend.key.size = unit(2,"line"),
        legend.title=element_text(size=16))


