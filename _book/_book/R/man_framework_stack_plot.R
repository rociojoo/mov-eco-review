#############################
# Making category line plots
#############################

library(tidyverse)
library(cowplot)
source("./R/expectation_functions.R")

# Arguments
path <- "./Data/ProcessedQueries/References/"
path.plots <- "./Rocio/Plots/"
path_processed_dictionaries <- "./Data/Dictionary/Papers-Term/"
path_dictionary_info <- "./Data/Dictionary/"

papers <- read.csv(file = paste0(path,"cleaned_papers_all_years_simple.csv"),stringsAsFactors = FALSE)

data_decade <- papers %>% 
  filter(pubyear > 2008 & pubyear < 2019)
######## Category #############

dictionary <- "Framework"
list_df <- expectation_functions(dictionary, data_all = data_decade, num_cat=NULL, ini_cat=1,
  paper_cat_out=TRUE, filter_lines=TRUE, suffix=NULL,
  path_processed_dictionaries)
corrected_df <- list_df[[1]]
num.var <- length(unique(corrected_df$category))
paper_cat <- list_df[[2]]

# creating a matrix for combinations of components
possibilities <- expand.grid(0:1, 0:1, 0:1, 0:1)
names(possibilities) <- c("External", "Internal", "Motion", "Navigation") #unique(corrected_df$category)

# filling it up with counts
possibilities$count <- sapply(1:dim(possibilities)[1], function(x){ sum(paper_cat[,1] == possibilities[x,1] & 
                                                                          paper_cat[,2] == possibilities[x,2] &
                                                                          paper_cat[,3] == possibilities[x,3] &
                                                                          paper_cat[,4] == possibilities[x,4])})

# computing percentages
possibilities$percentage <- round(possibilities$count / dim(paper_cat)[1] * 100,1)

# ordering the combinations based on the counts
possibilities_ordered <- possibilities[order(possibilities$count, decreasing = TRUE),]

# manually configurating the position of text for the squares in stack plots
possibilities_ordered$lab_ypos <- cumsum(possibilities_ordered$count) - 0.5 * possibilities_ordered$count

# transforming in a data frame form that ggplot likes
Components_combo <- gather(data = possibilities_ordered, key = "Component", value = "Transparency",
                           -count, -lab_ypos, -percentage)

# ordering the new data.frame
Components_combo <- Components_combo[order(Components_combo$count, decreasing = TRUE),]
# we won't put labels for small counts
Components_combo$lab_ypos[Components_combo$count < 200] <- NA

Components_combo$percentage <- paste0(Components_combo$percentage, "%")

ggplot(data = Components_combo, aes(x = Component, y = count)) +
  geom_col(aes(fill = Component, alpha = Transparency), colour = "black") +
  geom_text(aes(y = lab_ypos, label = percentage, group = Component, alpha = Transparency), color = "white") +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Number of papers") +
  scale_fill_manual(values=c('#77AADD','#EE8866','#BBCC33', '#FFAABB')) + 
  # scale_fill_brewer(palette = "Paired") +
  theme_cowplot(12) +
  theme(legend.position="none")

